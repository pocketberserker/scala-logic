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

object LogicT {

  implicit def logicTInstance[F[_]]: MonadPlus[LogicT[F, ?]] = new MonadPlus[LogicT[F, ?]] {

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

  implicit def logicTTrans: MonadTrans[LogicT] = new MonadTrans[LogicT] {
    def liftM[G[_] : Monad, A](m: G[A]) = new LogicT[G, A] {
      def apply[R](l: G[R])(f: A => G[R] => G[R]): G[R] = m.flatMap(a => f(a)(l))
    }
    def apply[G[_]: Monad] = logicTInstance[G]
  }
}
