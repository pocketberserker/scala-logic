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

trait MonadLogicInstances2 {

  implicit def writerTLogic[F[_], W](implicit L0: MonadLogic[F], M0: Monoid[W]): MonadLogic[WriterT[F, W, ?]] = new WriterTMonadLogic[F, W] {
    implicit def L: MonadLogic[F] = L0
    implicit def M: Monoid[W] = M0
  }
}

trait MonadLogicInstances1 extends MonadLogicInstances2 {

  import scalaz.StateT._

  implicit def stateTLogic[F[_], S](implicit L: MonadLogic[F]): MonadLogic[StateT[F, S, ?]] = new MonadLogic[StateT[F, S, ?]] {
    def point[A](a: => A) = stateTMonadPlus[S, F].point[A](a)
    def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]) = stateTMonadPlus[S, F].bind[A, B](fa)(f)
    def empty[A] = stateTMonadPlus[S, F].empty[A]
    def plus[A](a: StateT[F, S, A], b: => StateT[F, S, A]) = stateTMonadPlus[S, F].plus[A](a, b)

    def split[A](sm: StateT[F, S, A]) = StateT(s =>
      L.bind(L.split(sm.run(s))) {
        case None => L.pure((s, None))
        case Some(((s2, a), m)) => L.pure((s2, Some((a, StateT(Function.const(m))))))
      })

    override def interleave[A](m1: StateT[F, S, A], m2: StateT[F, S, A]): StateT[F, S, A] = StateT(s =>
      L.interleave(m1.run(s), m2.run(s))
    )

    override def >>-[A, B](m: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = StateT(s =>
      L.>>-(m.run(s)){ case (s2, a) => f(a).run(s2) }
    )

    override def ifte[A, B](t: StateT[F, S, A], el: StateT[F, S, B])(th: A => StateT[F, S, B]): StateT[F, S, B] =
      StateT(s => L.ifte(t.run(s), el.run(s)){ case (s2, a) => th(a).run(s2) })

    override def once[A](m: StateT[F, S, A]): StateT[F, S, A] = StateT(s => L.once(m.run(s)))
  }
}

trait MonadLogicInstances0 extends MonadLogicInstances1 {

  import scalaz.Kleisli._

  // MonadLogic[ReaderT[F, E, ?]]
  implicit def kleisliLogic[F[_], E](implicit L: MonadLogic[F]): MonadLogic[Kleisli[F, E, ?]] = new MonadLogic[Kleisli[F, E, ?]] {
    def point[A](a: => A) = kleisliMonadPlus[F, E].point[A](a)
    def bind[A, B](fa: Kleisli[F, E, A])(f: A => Kleisli[F, E, B]) = kleisliMonadPlus[F, E].bind[A, B](fa)(f)
    def empty[A] = kleisliMonadPlus[F, E].empty[A]
    def plus[A](a: Kleisli[F, E, A], b: => Kleisli[F, E, A]) = kleisliMonadPlus[F, E].plus[A](a, b)

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

private trait WriterTMonadLogic[F[_], W] extends MonadLogic[WriterT[F, W, ?]] {

  implicit def L: MonadLogic[F]
  implicit def M: Monoid[W]

  def tell(w: W): WriterT[F, W, Unit] = WriterT(L.pure((w, ())))

  def point[A](a: => A) = WriterT.writerTMonad[F, W].point[A](a)
  def bind[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]) = WriterT.writerTMonad[F, W].bind[A, B](fa)(f)
  def empty[A] = WriterT(L.empty[(W, A)])
  def plus[A](a: WriterT[F, W, A], b: => WriterT[F, W, A]) = WriterT(L.plus(a.run, b.run))

  def split[A](wm: WriterT[F, W, A]) = WriterT(
    L.bind(L.split(wm.run)) {
      case None => L.pure((M.zero, None))
      case Some(((w, a), m)) => L.pure((w, Some((a, WriterT(m)))))
    })

  override def interleave[A](m1: WriterT[F, W, A], m2: WriterT[F, W, A]): WriterT[F, W, A] =
    WriterT(L.interleave(m1.run, m2.run))

  override def >>-[A, B](m: WriterT[F, W, A])(f: A => WriterT[F, W, B]): WriterT[F, W, B] =
    WriterT(L.>>-(m.run){ case (w, a) => tell(w).flatMap(_ => f(a)).run })

  override def ifte[A, B](t: WriterT[F, W, A], el: WriterT[F, W, B])(th: A => WriterT[F, W, B]): WriterT[F, W, B] =
    WriterT(L.ifte(t.run, el.run){ case (w, a) => tell(w).flatMap(_ => th(a)).run })

  override def once[A](m: WriterT[F, W, A]): WriterT[F, W, A] = WriterT(L.once(m.run))
}
