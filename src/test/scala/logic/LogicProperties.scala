package logic

import scalaz.Equal
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop.forAll

object LogicProperties {

  object monadLogicLaw {

    def split[F[_], A](implicit M: MonadLogic[F], arbF: Arbitrary[F[A]], arbA: Arbitrary[A],
      arbTuple: Arbitrary[(A, F[A])], E: Equal[F[Option[(A, F[A])]]]) = new Properties("monad logic") {
      property("split empty") = E.equal(M.split(M.empty), M.pure(None))
      property("split values") = forAll { (a: A, m: F[A]) =>
        E.equal(M.split(M.plus(M.pure(a), m)), M.pure(Some((a, m))))
      }
    }
    def reflect[F[_], A](implicit M: MonadLogic[F], arbF: Arbitrary[F[A]],
      arbA: Arbitrary[A], E: Equal[F[A]]) = new Properties("monad logic") {
      property("reflect") = forAll { m: F[A] =>
        E.equal(M.bind(M.split(m))(MonadLogic.reflect(_)), m)
      }
    }
  }
}
