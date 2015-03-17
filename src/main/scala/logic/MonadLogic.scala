package logic

import scalaz._

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

object MonadLogic extends MonadLogicInstances with MonadLogicFunctions

trait MonadLogicFunctions {

  def reflect[F[_], A](a: Option[(A, F[A])])(implicit L: MonadLogic[F]): F[A] =
    a match {
      case None => L.empty
      case Some((a, m)) => L.plus(L.pure(a), m)
    }

  def lnot[F[_], A](m: F[A])(implicit L: MonadLogic[F]): F[Unit] =
    L.ifte(L.once(m), L.pure(()))(_ => L.empty)
}

trait MonadLogicInstances0 {

  import scalaz.Kleisli._

  // MonadLogic[ReaderT[F, E, ?]]
  implicit def kleisliLogic[F[_], E](implicit L: MonadLogic[F]): MonadLogic[Kleisli[F, E, ?]] = new MonadLogic[Kleisli[F, E, ?]] {
    def point[A](a: => A) = kleisliMonadPlus.point(a)
    def bind[A, B](fa: Kleisli[F, E, A])(f: A => Kleisli[F, E, B]) = kleisliMonadPlus.bind(fa)(f)
    def empty[A] = kleisliMonadPlus.empty[A]
    def plus[A](a: Kleisli[F, E, A], b: => Kleisli[F, E, A]) = kleisliMonadPlus.plus(a, b)

    def split[A](rm: Kleisli[F, E, A]) =
      Kleisli[F, E, Option[(A, Kleisli[F, E, A])]](e =>
        L.bind(L.split(rm.run(e))) {
          case None => L.pure(None)
          case Some((a, m)) => L.pure(Some((a, kleisliMonadTrans.liftM(m))))
        })
  }
}

trait MonadLogicInstances extends MonadLogicInstances0 {

  import scalaz.std.list.listInstance

  implicit val listLogic: MonadLogic[List] = new MonadLogic[List] {
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
