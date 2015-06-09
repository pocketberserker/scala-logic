package logic

import scalaz._
import scala.util.Random
import scalaprops._

object FunctionEqual extends FunctionEqual(30)

sealed class FunctionEqual(size: Int) {
  implicit def f1[A1: Gen, B](implicit B: Equal[B]): Equal[A1 => B] = {
    val values = Gen[A1].samples(listSize = size, seed = System.nanoTime)
    Equal.equal( (x, y) =>
      values.forall{ a => B.equal(x(a), y(a)) }
    )
  }

  implicit def f2[A1: Gen, A2: Gen, B](implicit B: Equal[B]): Equal[(A1, A2) => B] =
    f1[(A1, A2), B].contramap(_.tupled)

  implicit def f3[A1: Gen, A2: Gen, A3: Gen, B](implicit B: Equal[B]): Equal[(A1, A2, A3) => B] =
    f1[(A1, A2, A3), B].contramap(_.tupled)
}
