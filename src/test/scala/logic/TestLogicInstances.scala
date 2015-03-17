package logic

import scalaz._
import Id._
import org.scalacheck._
import org.scalacheck.Gen._

trait TestLogicInstances {

  def genLogic[A](implicit arb: Arbitrary[A]): Gen[Logic[A]] = for {
    xs <- Arbitrary.arbitrary[List[A]]
  } yield (new Logic[A] {
    def apply[R](l: Id[R])(f: A => Id[R] => Id[R]) =
      xs.foldRight(l)((a, b) => f(a)(b))
  })

  implicit def arbLogic[A](implicit arb: Arbitrary[A]): Arbitrary[Logic[A]] = Arbitrary(genLogic)

  implicit val arbIntLogic = arbLogic[Int]

  implicit def intLogicEqual(implicit E: Equal[List[Int]]): Equal[Logic[Int]] = new Equal[Logic[Int]] {
    override def equalIsNatural = E.equalIsNatural
    override def equal(a1: Logic[Int], a2: Logic[Int]) = E.equal(Logic.observeAll(a1), Logic.observeAll(a2))
  }
}
