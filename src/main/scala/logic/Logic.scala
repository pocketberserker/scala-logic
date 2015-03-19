package logic

import scalaz._
import Id._
import LogicT._

object Logic {

  def observe[A](l: Logic[A]): Option[A] = l.observe

  def observeAll[A](l: Logic[A]): List[A] = l.observeAll

  def observeMany[A](l: Logic[A], n: Int)(implicit L: MonadLogic[Logic]): List[A] =
    l.observeMany(n)

  implicit val logicTraverse: Traverse[Logic] = new Traverse[Logic] {
    def traverseImpl[F[_], A, B](l: Logic[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      def cons(b: B)(ll: Logic[B]): Logic[B] = logicTMonadPlus.plus(logicTMonadPlus.pure(b), ll)
      l(F.pure(logicTMonadPlus[Id].empty[B]))(a => ft => F.ap(ft)(F.map(f(a))(cons _)))
    }
  }
}
