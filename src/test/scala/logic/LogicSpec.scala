package logic

import scalaz._
import scalaz.std.AllInstances._
import FunctionEqual._
import scalaprops._
import scalaprops.ScalapropsScalaz._

sealed abstract class LogicSpec extends Scalaprops {

  protected[this] implicit def stateTEqual[F[_]: Monad, A, B](implicit F: Equal[A => F[(A, B)]]): Equal[StateT[F, A, B]] =
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
  val laws =
    Properties.either(
      "Logic",
      scalazlaws.monadPlus.all[Logic],
      scalazlaws.traverse.all[Logic]
    )
}

object ListLogicTest extends LogicSpec {
  val listMonadLogicLaws =
    MonadLogicLaw.laws[List]
}

object StateTLogicTest extends LogicSpec {
  val stateT =
    MonadLogicLaw.laws[StateT[List, List[Int], ?]]
      .ignore("original haskell implementation don't satisfy split value law...")
}

object KleisliLogicTest extends LogicSpec {
  val kleisli =
    MonadLogicLaw.laws[Kleisli[List, List[Int], ?]]
      .ignore("original haskell implementation don't satisfy split value law...")
}

object WriterTLogicTest extends LogicSpec {
  val writer =
    MonadLogicLaw.laws[WriterT[List, List[Int], ?]]
      .ignore("original haskell implementation don't satisfy reflect law...")
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
