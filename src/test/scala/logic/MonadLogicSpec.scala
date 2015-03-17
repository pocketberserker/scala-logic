package logic

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import Prop.forAll

object LogicProperties {

  object monadLogic {

    def check[F[_], A](implicit M: MonadLogic[F], arbF: Arbitrary[F[A]], arbA: Arbitrary[A], arbTuple: Arbitrary[(A, F[A])]) = new Properties("monad logic") {
      property("split empty") = (M.split(M.empty) == M.pure(None))
      property("split values") = forAll { (a: A, m: F[A]) =>
        M.split(M.plus(M.pure(a), m)) == M.pure(Some((a, m)))
      }
      property("reflect") = forAll { m: F[A] => M.bind(M.split(m))(MonadLogic.reflect(_)) == m }
    }
  }
}

class ListMonadLogicSpec extends SpecLite {
  checkAll(LogicProperties.monadLogic.check[List, AnyVal])
}
