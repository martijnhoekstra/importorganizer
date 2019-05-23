package fix
package fox

import scala.collection.immutable.HashSet
import scala.collection.immutable.List
import scala.collection.immutable.List.newBuilder
import scala.collection.immutable.Nil
import scala.collection.immutable.{Vector => Voctor}
import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future
import scala.util.Try

import com.oracle.util.Checksums

object Foo {
  implicit val ec = global
  //implicit val string2intOption = (str: String) => Try(str.toInt).toOption
  //doesn't produce the trees needed to find the import.
  //https://github.com/scalameta/scalameta/issues/1866
  implicit def string2intOption(str: String) = Try(str.toInt).toOption
}

object MyRepo {
  import Foo.ec
  import Foo.string2intOption
  val baz = 1 :: Nil
  val bar = baz.flatMap(i => baz) //implicit argument from predef
  val f = Future(1) //impicit imported
  val empt = Voctor.empty[Checksums]
  val b = newBuilder
  val hs: List[HashSet[Int]] = Nil
  //implicit conversion (view SLS 7.3)
  //implicit conversion 7.3.1
  val i: Option[Int] = "7"
  //implicit conversion 7.3.2
  //val seven = "7".get
  //implicit conversion 7.3.3
  //val fourteen = "7".flatMap((i: Int) => Some(i * 2))
}
