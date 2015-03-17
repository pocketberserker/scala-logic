package logic

import scalaz._
import LogicProperties._
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.tuple._
import scalaz.scalacheck.ScalazArbitrary._
import MonadLogic._

class MonadLogicSpec extends SpecLite {

  type KleisliList[A, B] = Kleisli[List, A, B]
  type KleisliListInt[B] = KleisliList[Int, B]

  checkAll(monadLogicLaw.split[List, Int])
  checkAll(monadLogicLaw.reflect[List, Int])

  implicit def KleisliListEqual(implicit M: Equal[List[Int]]): Equal[Kleisli[List, Int, Int]] = new Equal[Kleisli[List, Int, Int]] {
    def equal(a1: Kleisli[List, Int, Int], a2: Kleisli[List, Int, Int]) = {
      val mb1 = a1.run(0)
      val mb2 = a2.run(0)
      M.equal(mb1, mb2)
    }
  }

  implicit def KleisliListOptEqual(implicit M: Equal[List[Option[(Int, Kleisli[List, Int, Int])]]])
    : Equal[Kleisli[List, Int, Option[(Int, Kleisli[List, Int, Int])]]] = new Equal[Kleisli[List, Int, Option[(Int, Kleisli[List, Int, Int])]]] {
    def equal(a1: Kleisli[List, Int, Option[(Int, Kleisli[List, Int, Int])]], a2: Kleisli[List, Int, Option[(Int, Kleisli[List, Int, Int])]]) = {
      val mb1 = a1.run(0)
      val mb2 = a2.run(0)
      M.equal(mb1, mb2)
    }
  }

  checkAll(monadLogicLaw.split[KleisliListInt, Int])
  checkAll(monadLogicLaw.reflect[KleisliListInt, Int])
}
