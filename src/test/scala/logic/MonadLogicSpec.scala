package logic

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.FunSpec
import org.scalatest.prop.Checkers

abstract class MonadLogicSpec[F[_], A](implicit M: MonadLogic[F],
  arbF: Arbitrary[F[A]], arbA: Arbitrary[A], arbTuple: Arbitrary[(A, F[A])]) extends FunSpec with Checkers {

  describe("MonadLogic") {

    it("split") {
      check(M.split(M.empty) == M.pure(None))
      check { (a: A, m: F[A]) =>
        M.split(M.plus(M.pure(a), m)) == M.pure(Some((a, m)))
      }
    }

    it("reflect") {
      check { m: F[A] => M.bind(M.split(m))(MonadLogic.reflect(_)) == m }
    }
  }
}

class ListMonadLogicSpec extends MonadLogicSpec[List, AnyVal]
