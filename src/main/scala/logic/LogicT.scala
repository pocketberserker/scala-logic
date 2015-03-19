package logic

import scalaz._
import scalaz.syntax.bind._

trait LogicT[F[_], A] {

  def apply[R](l: F[R])(f: A => F[R] => F[R]): F[R]

  def observe(implicit M: Applicative[F]): F[Option[A]] =
    this(M.pure(None: Option[A]))(a => Function.const(M.pure(Some(a))))

  def observeAll(implicit M: Applicative[F]): F[List[A]] =
    this(M.pure(Nil: List[A]))(a => b => M.map(b)(a :: _))

  def observeMany(n: Int)(implicit M: Applicative[F], L: MonadLogic[LogicT[F, ?]]): F[List[A]] = {
    def sk(o: Option[(A, LogicT[F, A])])(a: Any): F[List[A]] = o match {
      case None => M.pure(Nil)
      case Some((a, m)) => M.map(m.observeMany(n - 1))(a :: _)
    }
    if(n <= 0) M.pure(Nil)
    else if(n == 1) this(M.pure(Nil: List[A]))(a => Function.const(M.pure(List(a))))
    else L.split(this)(M.pure(Nil: List[A]))(sk)
  }
}

object LogicT extends LogicTInstances

sealed abstract class LogicTInstances3 {

  implicit def logicTMonadPlus[F[_]]: MonadPlus[LogicT[F, ?]] = new MonadPlus[LogicT[F, ?]] {

    override def map[A, B](lt: LogicT[F, A])(f: A => B) = new LogicT[F, B] {
      def apply[R](l: F[R])(sk: B => F[R] => F[R]) = lt(l)(a => sk(f(a)))
    }

    def point[A](a: => A) = new LogicT[F, A] {
      def apply[R](fk: F[R])(sk: A => F[R] => F[R]) = sk(a)(fk)
    }

    def bind[A, B](fa: LogicT[F, A])(f: A => LogicT[F, B]) = new LogicT[F, B] {
      def apply[R](fk: F[R])(sk: B => F[R] => F[R]) = fa(fk)(a => fkk => f(a)(fkk)(sk))
    }

    def empty[A] = new LogicT[F, A] {
      def apply[R](fk: F[R])(sk: A => F[R] => F[R]) = fk
    }

    def plus[A](a: LogicT[F, A], b: => LogicT[F, A]) = new LogicT[F, A] {
      def apply[R](fk: F[R])(sk: A => F[R] => F[R]) = a(b(fk)(sk))(sk)
    }
  }

  implicit def logicTMonadTrans: MonadTrans[LogicT] = new MonadTrans[LogicT] {
    def liftM[G[_] : Monad, A](m: G[A]) = new LogicT[G, A] {
      def apply[R](l: G[R])(f: A => G[R] => G[R]): G[R] = m.flatMap(a => f(a)(l))
    }
    def apply[G[_]: Monad] = logicTMonadPlus[G]
  }

  implicit def logicTFoldable[F[_]](implicit T: Foldable[F], S: Applicative[F]): Foldable[LogicT[F, ?]] = new Foldable[LogicT[F, ?]] {

    def foldMap[A, B](fa: LogicT[F, A])(f: A => B)(implicit M: Monoid[B]) =
      T.fold(fa(S.pure(M.zero))(a => b => S.map(b)(M.append(f(a), _))))

    def foldRight[A, B](fa: LogicT[F, A], z: => B)(f: (A, => B) => B) =
      foldMap(fa)((a: A) => (Endo.endo(f(a, _: B)))) apply z
  }
}

sealed abstract class LogicTInstances2 extends LogicTInstances3 {

  implicit def logicTMonadLogic[F[_]](implicit L: MonadLogic[F]): MonadLogic[LogicT[F, ?]] = new MonadLogic[LogicT[F, ?]] {

    override def map[A, B](lt: LogicT[F, A])(f: A => B) = logicTMonadPlus.map(lt)(f)
    def point[A](a: => A) = logicTMonadPlus.point(a)
    def bind[A, B](fa: LogicT[F, A])(f: A => LogicT[F, B]) = logicTMonadPlus.bind(fa)(f)
    def empty[A] = logicTMonadPlus.empty[A]
    def plus[A](a: LogicT[F, A], b: => LogicT[F, A]) = logicTMonadPlus.plus(a, b)

    def split[A](l: LogicT[F, A]) = {
      def ssk(a: A)(fk: F[Option[(A, F[A])]]): F[Option[(A, F[A])]] =
        L.pure(Some((a, L.bind(fk)(b => MonadLogic.reflect(b)))))
      val s: F[Option[(A, F[A])]] = L.pure(None)
      logicTMonadTrans.liftM(L.map(l(s)(ssk _))(o => o.map { case (a, ll) =>
        (a, logicTMonadTrans.liftM(ll))
      }))
    }
  }
}

sealed abstract class LogicTInstances1 extends LogicTInstances2 {

  def logicTMonadReader[F[_, _], R](implicit F0: MonadReader[F, R]): MonadReader[({ type G[A, B] = LogicT[({ type H[T] = F[A, T] })#H, B] })#G, R] =
    new LogicTMonadReader[F, R] {
      implicit def F: MonadReader[F, R] = F0
    }
}

sealed abstract class LogicTInstances0 extends LogicTInstances1 {
  def logicTMonadState[F[_, _], S](implicit F0: MonadState[F, S]): MonadState[({ type G[A, B] = LogicT[({ type H[T] = F[A, T] })#H, B] })#G, S] =
    new LogicTMonadState[F, S] {
      implicit def F: MonadState[F, S] = F0
    }
}

sealed abstract class LogicTInstances extends LogicTInstances0 {
  def logicTMonadError[F[_, _], E](implicit F0: MonadError[F, E]): MonadError[({ type G[A, B] = LogicT[({ type H[T] = F[A, T] })#H, B] })#G, E] =
    new LogicTMonadError[F, E] {
      implicit def F: MonadError[F, E] = F0
    }
}

private trait LogicTMonadReader[F[_, _], R] extends MonadReader[({ type G[A, B] = LogicT[({ type H[T] = F[A, T] })#H, B] })#G, R] {

  implicit def F: MonadReader[F, R]

  type G[X] = F[R, X]

  def point[A](a: => A) = LogicT.logicTMonadTrans.liftM[G, A](F.point(a))
  def bind[A, B](fa: LogicT[G, A])(f: A => LogicT[G, B]): LogicT[G, B] = LogicT.logicTMonadPlus.bind(fa)(f)

  def ask = LogicT.logicTMonadTrans.liftM[G, R](F.ask)
  def local[A](f: R => R)(m: LogicT[G, A]): LogicT[G, A] = new LogicT[G, A] {
    def apply[X](l: G[X])(sk: A => G[X] => G[X]) =
      m(F.local(f)(l))(x => sk(x))
  }
}

private trait LogicTMonadState[F[_, _], S] extends MonadState[({ type G[A, B] = LogicT[({ type H[T] = F[A, T] })#H, B] })#G, S] {

  implicit def F: MonadState[F, S]

  type G[X] = F[S, X]

  def point[A](a: => A) = LogicT.logicTMonadTrans.liftM[G, A](F.point(a))
  def bind[A, B](fa: LogicT[G, A])(f: A => LogicT[G, B]): LogicT[G, B] = LogicT.logicTMonadPlus.bind(fa)(f)

  def init = LogicT.logicTMonadTrans.liftM[G, S](F.init)
  def get = LogicT.logicTMonadTrans.liftM[G, S](F.get)
  def put(s: S) = LogicT.logicTMonadTrans.liftM[G, Unit](F.put(s))
}

private trait LogicTMonadError[F[_, _], E] extends MonadError[({ type G[A, B] = LogicT[({ type H[T] = F[A, T] })#H, B] })#G, E] {

  implicit def F: MonadError[F, E]

  type G[X] = F[E, X]

  def point[A](a: => A) = LogicT.logicTMonadTrans.liftM[G, A](F.point(a))
  def bind[A, B](fa: LogicT[G, A])(f: A => LogicT[G, B]): LogicT[G, B] = LogicT.logicTMonadPlus.bind(fa)(f)

  def raiseError[A](e: E) = LogicT.logicTMonadTrans.liftM[G, A](F.raiseError(e))
  def handleError[A](l: LogicT[G, A])(f: E => LogicT[G, A]): LogicT[G, A] = new LogicT[G, A] {
    def apply[X](fk: G[X])(sk: A => G[X] => G[X]) = {
      def handle(r: G[X]): G[X] = F.handleError(r)(e => f(e)(fk)(sk))
      handle(l(fk)(a => x => sk(a)(handle(x))))
    }
  }
}
