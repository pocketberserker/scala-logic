package logic

import scalaz._
import scalaz.std.list.listInstance

trait MonadLogic[F[_]] extends MonadPlus[F] {

  private def maybe[A, B](m: Option[A], default: => B)(f: A => B): B =
    m match {
      case None => default
      case Some(a) => f(a)
    }

  def split[A](m: F[A]): F[Option[(A, F[A])]]

  def interleave[A](m1: F[A], m2: F[A]): F[A] =
    bind(split(m1))(maybe(_, m2){ case (a, m1a) => plus(pure(a), interleave(m2, m1a)) })

  def >>-[A, B](m: F[A])(f: A => F[B]): F[B] =
    bind(bind(split(m))(maybe(_, empty[(A, F[A])])(pure(_)))) { case (a, m1) =>
      interleave(f(a), >>-(m1)(f))
    }

  def ifte[A, B](t: F[A], el: F[B])(th: A => F[B]): F[B] =
    bind(split(t))(maybe(_, el){ case (a, m) => plus(th(a), bind(m)(th)) })

  def once[A](m: F[A]): F[A] =
    bind(bind(split(m))(maybe(_, empty[(A, F[A])])(pure(_)))) { case (a, _) => pure(a) }
}

object MonadLogic {

  def reflect[F[_], A](a: Option[(A, F[A])])(implicit L: MonadLogic[F]): F[A] =
    a match {
      case None => L.empty
      case Some((a, m)) => L.plus(L.pure(a), m)
    }

  def lnot[F[_], A](m: F[A])(implicit L: MonadLogic[F]): F[Unit] =
    L.ifte(L.once(m), L.pure(()))(_ => L.empty)

  implicit val listLogic: MonadLogic[List] =
    new MonadLogic[List] {
      def split[A](l: List[A]) = l match {
        case Nil => pure(None)
        case x::xs => pure(Some((x, xs)))
      }
      def point[A](a: => A) = listInstance.point(a)
      def bind[A, B](fa: List[A])(f: A => List[B]) = listInstance.bind(fa)(f)
      def empty[A] = listInstance.empty[A]
      def plus[A](a: List[A], b: => List[A]) = listInstance.plus(a, b)
    }
}
