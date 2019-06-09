package logic

import scalaz._
import scalaprops._

object MonadLogicLaw {

  def splitEmpty[F[_], A](implicit M: MonadLogic[F], E: Equal[F[Option[(A, F[A])]]]) =
    Property.prop(E.equal(M.split(M.empty), M.pure(None)))

  def splitValues[F[_], A](implicit M: MonadLogic[F], F: Gen[F[A]], A: Gen[A],
                           E: Equal[F[Option[(A, F[A])]]]) =
    Property.forAll { (a: A, m: F[A]) =>
      E.equal(M.split(M.plus(M.pure(a), m)), M.pure(Some((a, m))))
    }

  def reflect[F[_], A](implicit M: MonadLogic[F], F: Gen[F[A]],
                       A: Gen[A], E: Equal[F[A]]) =
    Property.forAll { m: F[A] =>
      E.equal(M.bind(M.split(m))(MonadLogic.reflect(_)), m)
    }

  def laws[F[_]](implicit
                 M: MonadLogic[F],
                 F: Gen[F[Int]],
                 E1: Equal[F[Option[(Int, F[Int])]]],
                 E2: Equal[F[Int]]
                  ) = Properties.properties("monadLogic")(
    "split empty" -> splitEmpty[F, Int],
    "split values" -> splitValues[F, Int],
    "reflect" -> reflect[F, Int]
  )
}
