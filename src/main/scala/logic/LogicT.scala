package logic

import scalaz._
import scalaz.syntax.bind._

trait LogicT[F[_], A] {

  def apply[R](l: F[R])(f: A => F[R] => F[R]): F[R]

  def observe(implicit M: MonadPlus[F]): F[A] =
    this(M.empty)(a => Function.const(M.pure(a)))

  def observeAll(implicit M: Monad[F]): F[List[A]] =
    this(M.pure(Nil: List[A]))(a => b => M.map(b)(a :: _))

  def observeMany(n: Int)(implicit M: Monad[F], L: MonadLogic[LogicT[F, ?]]): F[List[A]] = {
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

sealed abstract class LogicTInstances0 {

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

  implicit def logicTFoldable[F[_]](implicit T: Foldable[F], S: Monad[F]): Foldable[LogicT[F, ?]] = new Foldable[LogicT[F, ?]] {

    def foldMap[A, B](fa: LogicT[F, A])(f: A => B)(implicit M: Monoid[B]) =
      T.fold(fa(S.pure(M.zero))(a => b => S.map(b)(M.append(f(a), _))))

    def foldRight[A, B](fa: LogicT[F, A], z: => B)(f: (A, => B) => B) =
      foldMap(fa)((a: A) => (Endo.endo(f(a, _: B)))) apply z
  }
}

sealed abstract class LogicTInstances extends LogicTInstances0 {
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
