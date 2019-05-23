package fix

import scalafix.v1._
import scala.meta._

object ImportOrdering {
  def zipOption[A, B](as: List[A], bs: List[B]) = as.map(Some(_)).zipAll(bs.map(Some(_)), None, None)

  def unprefix[A](as1: List[A], as2: List[A]) = zipOption(as1, as2).dropWhile{ case (o1, o2) => o1 == o2}.unzip match {
      case (l1, l2) => (l1.collect{ case Some(v1) => v1}, l2.collect { case Some(v2) => v2})
  }

  def unprefixBy[A, B](as1: List[A], as2: List[A], projection: A => B) = zipOption(as1, as2).dropWhile {
    case (Some(a1), Some(a2)) if projection(a1) == projection(a2) => true
    case _ => false
  }.unzip match {
    case (l1, l2) => (l1.collect{ case Some(v1) => v1}, l2.collect { case Some(v2) => v2})
  }
  
  def byElem[A](implicit oa: Ordering[A]) = new Ordering[List[A]] {
    def compare(x: List[A], y: List[A]): Int = (x, y) match {
      case (Nil, Nil) => 0
      case (_, Nil)   => 1
      case (Nil, _)   => -1
      case (h1 :: t1, h2 :: t2) => {
        oa.compare(h1, h2) match {
          case 0 => compare(t1, t2)
          case c => c
        }
      }
    }
  }

  /* this doesn't resove as one would hope
  implicit def listorder[A: Ordering] = Ordering.by{
      case Nil => (None, None)
      case h :: t => (Some(h), Some(t))
  }
  */

  def importeeOrder(implicit on: Ordering[Name]) =
    Ordering.by((imp: Importee) =>
      imp match {
        case Importee.Name(nme)        => (0, Some(nme), None)
        case Importee.Rename(from, to) => (0, Some(from), Some(to))
        case w: Importee.Wildcard      => (2, None, None)
        case Importee.Unimport(from)   => (3, Some(from), None)
    })

  def selectChain(r: Term.Ref): Stream[Name] = r match {
    case n: Name                     => n #:: Stream.empty
    case Term.Select(t: Term.Ref, n) => n #:: selectChain(t)
  }
  def selectList(r: Term.Ref) = selectChain(r).reverseIterator.toList

  def refOrder(implicit ord: Ordering[List[Name]]) = Ordering.by(selectList)

  def importerOrder(implicit ieeo: Ordering[Importee], nameorder: Ordering[Name]) = new Ordering[Importer] {
    def compare(i1: Importer, i2: Importer): Int = {
      def compareRemainders(remainder1: List[Name], remainder2: List[Name], importee1: Importee, importee2: Importee): Int = {
        (remainder1, remainder2) match {
          //import from the same thing
          case (Nil, Nil) => ieeo.compare(importee1, importee2) 
          case (head :: t, Nil) => {
            importee2 match {
              case Importee.Name(nme) => {
                val cmp = nameorder.compare(head, nme)
                if (cmp == 0) 1 //sort foo.bar.baz after foo.bar
                else cmp //sort foo.baz beetween foo.bar.x and foo.foo.x
              }
              case Importee.Rename(from, to) => {
                val cmp = nameorder.compare(head, from)
                if (cmp == 0) 1 //sort foo.bar.baz after foo.{bar => bas}
                else cmp //sort foo.{baz => xxx} beetween foo.bar.x and foo.foo.x
              }
              case _: Importee.Wildcard => 1 //sort foo.bar.baz after foo._
              case Importee.Unimport(from) => {
                val cmp = nameorder.compare(head, from)
                if (cmp == 0) -1 //sort foo.bar.baz before foo.{bar => _}
                else cmp //sort foo.{baz => _} beetween foo.bar.x and foo.foo.x
              }
            }
          }
          case (Nil, l) => -compareRemainders(l, Nil, importee2, importee1)
          case (h1 :: _, h2 :: _) =>nameorder.compare(h1, h2)
        }
      }
      val (r1, r2) = unprefixBy(selectList(i1.ref), selectList(i2.ref), (nme: Name) => nme.value)
      compareRemainders(r1, r2, i1.importees.head, i2.importees.head)
    }
  }
}

