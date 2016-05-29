package jto.validation

trait At[F[_]] {
  def at[A](path: Path, f: F[A]): F[A]
}

object At {
  def apply[F[_]](implicit at: At[F]) = at
}
