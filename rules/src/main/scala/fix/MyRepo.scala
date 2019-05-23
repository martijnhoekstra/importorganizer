package fix

import scalafix.v1._
import scala.meta._

class MyRepo extends SemanticRule("MyRepo") {

  override def fix(implicit doc: SemanticDocument): Patch = {

    val maxNamedImportees = 3

    def ancesters(trm: Tree): Stream[Tree] = trm.parent match {
      case None => Stream.empty
      case Some(parent) => parent #:: ancesters(parent)
    }

    def scope(importee: Importee) = for {
      importer <- importee.parent
      impor <- importer.parent
      enclosing <- impor.parent
    } yield enclosing

    def isInScopeOf(tree: Tree, importee: Importee) = {
      ancesters(tree).map(Some.apply).contains(scope(importee)) &&
      importee.pos.end <= tree.pos.start
    }

    def importBlock(initial: Import): Vector[Import] = {
      initial.parent.toVector.flatMap(p => 
        p.children.dropWhile(t => t != initial)
        .takeWhile(t => t.is[Import])
      ).asInstanceOf[Vector[Import]]
    }

    def refNames(ref: Term): Stream[Name] = ref match {
      case Term.Select(reff, nme) => nme #:: refNames(reff)
      case n @ Name(nme) => n #:: Stream.empty
    }

    def isJavaImport(imp: Import)(implicit doc: SemanticDocument) = imp.toString().startsWith("import java.")
    def isScalaImport(imp: Import)(implicit doc: SemanticDocument) = imp.toString().startsWith("import scala.")
    def libname(imp: Import)(implicit doc: SemanticDocument): String = refNames(imp.importers.head.ref).reverse.toList match {
      case (_ :: tld :: org :: tl) if isTld(tld.value) => org.value
      case (_ :: org :: tl) => org.value
      case l => {
        println(s"what import? $imp ${l.map(_.value)}") 
        ""
      }
    }

    def isTld(name: String) = name match {
      case "com" | "net" | "org" => true
      case _ => false
    }

    def owners(sym: Symbol): Stream[Symbol] = (sym.owner #:: owners(sym.owner)).takeWhile(owner => !owner.isNone)

    def importBlocks(from: Tree): Vector[Vector[Import]] = {
      var outer: Vector[Vector[Import]] = Vector.empty
      def currentBlock = outer.lastOption.toVector.flatten
      from.traverse{
        case i: Import if !currentBlock.contains(i) => {
          outer :+= importBlock(i)
        }
      }
      outer
    }

    val importOrder: Ordering[Import] = {
      import ImportOrdering._
      implicit val nameOrder = Ordering.by((nme: Name) => nme.value)
      implicit val iee = importeeOrder
      implicit val io = importerOrder

      
      Ordering.by((imp: Import) => imp.importers.headOption)
    }
    
    def orderImportBlock(imports: Vector[Import]): Seq[Seq[Import]] = {
      val (javablock, rest) = imports.partition(isJavaImport)
      val (scalablock, external) = rest.partition(isScalaImport)
      val byLib = external.groupBy(libname)
      val blocks = javablock :: scalablock :: byLib.toList.sortBy(_._1).map(_._2)
      blocks.map(_.sorted(importOrder)).filterNot(_.isEmpty)
    }

    def zipAllOption[A, B](as: List[A], bs: Iterable[B]) = as.map(a => Some(a)).zipAll(bs.map(b => Some(b)), None, None)

    def buildImport(symbol: Symbol, named: Option[String], packges: List[Term.Ref]) = {
      val namedImportee = Importee.Name(Name(symbol.displayName))
      val importee = named.foldLeft[Importee](namedImportee)(rename)
      val symbolOwners = owners(symbol).map(_.displayName).reverse.toList.drop(1)
      //this is not exactly right: if the name is from a different compilation unit
      //and is also imported explicitly, then we can't omit the packages.
      val prefix @ (h :: t) = zipAllOption(symbolOwners, packges).dropWhile{
        case (Some(s), Some(Term.Name(nme))) => s == nme
        case (Some(s), Some(Term.Select(_, nme))) => s == nme
        case _ => false
      }.collect { case (Some(x), _) => x }
      val ref = t.foldLeft(Term.Name(h): Term.Ref)((n, s) => Term.Select(n, Term.Name(s)))
      Import(List(Importer(ref, List(importee))))
    }

    def rename(importee: Importee, name: String) = importee match {
      case n @ Importee.Name(Name(`name`)) =>  n
      case Importee.Name(n) => Importee.Rename(n, Name(name))
      case Importee.Rename(n, _) => Importee.Rename(n, Name(name))
      case _: Importee.Wildcard => ???
      case _: Importee.Unimport => ???
    }

    def syntheticNames(trm: Term) = {

      def syntheticNamesSem(semt: SemanticTree): List[(Term, Symbol)] = {
        semt match {
          case idt @ IdTree(info) => List((trm, idt.symbol))
          case appl @ ApplyTree(IdTree(info), _) if info.displayName == "apply" => {
            println(s"is $appl with structure ${appl.structure} my synth?")
            println(info)
            appl.structure
            val res = appl.symbol.toList.map(sym => (trm, sym.owner))
            res
          }
          case ApplyTree(fun, args) => (fun :: args).flatMap(syntheticNamesSem)
          case _ => {
            println(s"unexpected tree: $semt")
            Nil
          }
        }
      }
      trm.synthetic.toList.flatMap(syntheticNamesSem)
    }

    val updates = {
      val replacements = importBlocks(doc.tree).map(imps => (imps, scala.collection.mutable.Set.empty[Import]))
      
      val namedSymbols = doc.tree.collect {
        case nme: Name if ancesters(nme).collectFirst{ case Import(_) => ()}.isEmpty => List((nme, nme.symbol))
        case trm: Term => syntheticNames(trm)
      }.flatten
      
      namedSymbols.foreach{ case (tree, sym) =>
        for {
          (block, target) <- replacements.reverseIterator
          imp <- block.reverseIterator
          Importer(ref, importees) <- imp.importers.reverseIterator
          importee <- importees.reverseIterator.collectFirst {
            //Todo: if there is an import that imports the same name explicitly
            //and this is a wildcard import, making the import explicit
            //will make imports ambiguous.
            //e.g. you can't re-write
            // import foo._
            // val a = x 
            // import bar.x //or import bar.{y => x}
            // val b = x
            // to 
            // import foo.x
            // val a = x
            // import bar.x
            // val b = x //error, "reference to x is ambiguous"
            //IOW: can't make a wildcard into a named import if the same name is later used
            //from a different (named) import/symbol in the same scope
            //IOW: when moving back, when I encounter a wildcard import, I can't make that
            //into a named import if I already have a named import for that name, if that other imports names are in scope of this import.
            //N.B.  scala/bug#9208 eats our lunch. For now, leave that to upstream
            case i @ Importee.Name(Name(named)) if named == "ec" && i.symbol == sym && isInScopeOf(tree, i) =>  println("this!")
            case i @ Importee.Name(Name(named)) if i.symbol == sym && isInScopeOf(tree, i) =>  ()
            case i: Importee.Rename if i.symbol == sym && isInScopeOf(tree, i) => ()
            case i: Importee.Wildcard if ref.symbol == sym.owner && isInScopeOf(tree, i) => ()
          }
        } {
          val pkg = ancesters(tree).collect { case Pkg(ref, _) => ref}.reverse.toList
          val built = tree match {
            case Name(str) => buildImport(sym, Some(str), pkg)
            case _ => buildImport(sym, None, pkg)
          }
          println
          println(s"adding import $built for tree $tree to block $block")
          target.retain(cur => cur.toString != built.toString)
          target.add(built)
        }
      }

      replacements.map{ case (old, replacement) => {
        val importers = for {
          imp <- replacement.iterator
          imper <- imp.importers
          impee <- imper.importees
        } yield impee

        var grouped = replacement.toVector.flatMap(imprt => {
            val byRef = imprt.importers.groupBy(imper => imper.ref)
            byRef.map {
              case (ref, importers) => {
                val individuals = importers.flatMap(_.importees)
                val target: List[Importee] = if (individuals.length <= maxNamedImportees) {
                  individuals
                } else List(Importee.Wildcard())
                Import(List(Importer(ref, target)))
              }
            }
          })
          
        var ordered = orderImportBlock(grouped)
        (old, ordered)
      }}
    }

    def rebuildImportBlock(from: Seq[Import], to: Seq[Seq[Import]]): Patch = {
      //should find indent before first import, and place that before each string
      //also, should remove leftover whitespace tokens that remain from removing current imports.
      //my kingdom for a cursor-like API with leftTree - rightTree and tokensBefore - tokensAfter
      val h = from.head
      val indent = h.pos.startColumn
      val sep = "\n" + (" " * indent)
      val importBlock = to.map(subblock => subblock.mkString(sep)).mkString("\n\n")
      Patch.addLeft(h, importBlock) ++ from.map(imp => Patch.removeTokens(imp.tokens))
    }

    updates.foldLeft(Patch.empty) {
      case (patch, (from, to)) => patch + rebuildImportBlock(from, to)
    }

    
  }

}
