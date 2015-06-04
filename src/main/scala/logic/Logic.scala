package logic

import scalaz._

object Logic {

  def observe[A](l: Logic[A]): Option[A] = l.observe

  def observeAll[A](l: Logic[A]): List[A] = l.observeAll

  def observeMany[A](l: Logic[A], n: Int): List[A] =
    l.observeMany(n)
}
