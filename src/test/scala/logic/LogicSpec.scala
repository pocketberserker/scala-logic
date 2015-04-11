package logic

import scalaz._
import scalaz.std.AllInstances._
import FunctionEqual._
import scalaprops._

sealed abstract class LogicSpec extends Scalaprops{

  protected[this] implicit def stateTEqual[F[_], A, B](implicit F: Equal[A => F[(A, B)]]): Equal[StateT[F, A, B]] =
    F.contramap(_.apply _)

  protected[this] implicit def kleisliEqual[F[_], A: Gen, B](implicit E: Equal[F[B]]): Equal[Kleisli[F, A, B]] =
    Equal[A => F[B]].contramap(_.run)

  protected[this] implicit def logicGen[A: Gen]: Gen[Logic[A]] =
    Gen[List[A]].map(xs =>
      new Logic[A] {
        def apply[R](l: R)(f: A => R => R) =
          xs.foldRight(l)((a, b) => f(a)(b))
      }
    )

  protected[this] implicit def logicEqual[A: Equal] =
    Equal.equal[Logic[A]] { (a, b) =>
      import scalaz.syntax.equal._
      val f1 = (l: Logic[A]) => l.observe
      val f2 = (l: Logic[A]) => l.observeAll
      (f1(a) === f1(b)) && (f2(a) === f2(b))
    }
}

object LogicTest extends LogicSpec{
  val testLaws =
    Properties.either(
      "Logic",
      scalazlaws.monadPlus.all[Logic],
      scalazlaws.traverse.all[Logic]
    )
}

object ListLogicTest extends LogicSpec {
  val testListMonadLogicLaws =
    MonadLogicLaw.laws[List]
}

object StateTLogicTest extends LogicSpec {
  val testStateT =
    MonadLogicLaw.laws[StateT[List, Int, ?]]
}

object KleisliLogicTest extends LogicSpec {
  val testKleisli =
    MonadLogicLaw.laws[Kleisli[List, Int, ?]]
}

object WriterTLogicTest extends LogicSpec {
  val testWriter =
    MonadLogicLaw.laws[WriterT[List, Int, ?]]
}

object InstancesTest {
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
