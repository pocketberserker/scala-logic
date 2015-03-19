package logic
package syntax

import scalaz.syntax._

final class MonadLogicOps[F[_],A] private[syntax](val self: F[A])(implicit val F: MonadLogic[F]) extends Ops[F[A]] {

  final def split = F.split(self)

  final def interleave(other: F[A]) = F.interleave(self, other)

  final def >>-[B](f: A => F[B]) = F.>>-(self)(f)

  final def ifte[B](el: F[B])(th: A => F[B]) = F.ifte(self, el)(th)

  final def once = F.once(self)

  final def lnot = MonadLogic.lnot(self)
}

trait ToMonadLogicOps extends ToMonadPlusOps {

  implicit def ToMonadLogicOps[F[_], A](v: F[A])(implicit F0: MonadLogic[F]) =
    new MonadLogicOps[F,A](v)
}

trait MonadLogicSyntax[F[_]] extends  MonadPlusSyntax[F] {

  implicit def ToMonadLogicOps[A](v: F[A]): MonadLogicOps[F, A] =
    new MonadLogicOps[F, A](v)(MonadLogicSyntax.this.F)

  def F: MonadLogic[F]
}
