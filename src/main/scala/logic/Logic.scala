package logic

import scalaz._
import Id._

object Logic {

  def observe[A](l: Logic[A])(implicit M: MonadPlus[Id]): Id[A] = l.observe

  def observeAll[A](l: Logic[A])(implicit M: Monad[Id]): Id[List[A]] = l.observeAll

  def observeMany[A](l: Logic[A], n: Int)(implicit M: Monad[Id], L: MonadLogic[Logic[?]]): Id[List[A]] =
    l.observeMany(n)
}
