package logic

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.scalacheck.ScalazProperties._
import Logic._

object LogicSpec extends SpecLite with TestLogicInstances {

  checkAll(monadPlus.strongLaws[Logic])
  checkAll(traverse.laws[Logic])

  object instances {
    def functor[F[_] : Functor] = Functor[LogicT[F, ?]]
    def apply[F[_] : Apply] = Apply[LogicT[F, ?]]
    def plus[F[_] : Plus] = Plus[LogicT[F, ?]]
    def empty[F[_] : PlusEmpty] = PlusEmpty[LogicT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[LogicT[F, ?]]
    def apply[F[_] : Monad] = Apply[LogicT[F, ?]]
    def plus[F[_] : PlusEmpty] = Plus[LogicT[F, ?]]
    def empty[F[_] : MonadPlus] = PlusEmpty[LogicT[F, ?]]
  }
}
