package jto.validation

object Build {
  def apply[F1[_]: At, A](as: As1[F1] => F1[A]): F1[A] =
    as(As1[F1](Path()))

  def apply[F1[_]: At, F2[_]: At, A](as: As2[F1, F2] => F1[A] with F2[A])
    (implicit M: Mixer2[F1, F2]): F1[A] with F2[A] =
      as(As2[F1, F2](Path()))

  def apply[F1[_]: At, F2[_]: At, F3[_]: At, A](as: As3[F1, F2, F3] => F1[A] with F2[A] with F3[A])
    (implicit M: Mixer3[F1, F2, F3]): F1[A] with F2[A] with F3[A] =
      as(As3[F1, F2, F3](Path()))
}

case class As1[F1[_]: At](path: Path) {
  def as[A](m1: F1[A]): F1[A]= At[F1].at(path, m1)

  def \(key: String): As1[F1] = As1(path \ key)
  def \(idx: Int): As1[F1] = As1(path \ idx)
  def \(child: PathNode): As1[F1] = As1(path \ child)
}

case class As2[F1[_]: At, F2[_]: At](path: Path)(implicit M: Mixer2[F1, F2]) {
  def as[A](m1: F1[A], m2: F2[A]): F1[A] with F2[A] =
    M.mix(At[F1].at(path, m1), At[F2].at(path, m2))

  def \(key: String): As2[F1, F2] = As2(path \ key)
  def \(idx: Int): As2[F1, F2] = As2(path \ idx)
  def \(child: PathNode): As2[F1, F2] = As2(path \ child)
}

case class As3[F1[_]: At, F2[_]: At, F3[_]: At](path: Path)(implicit M: Mixer3[F1, F2, F3]) {
  def as[A](m1: F1[A], m2: F2[A], m3: F3[A]): F1[A] with F2[A] with F3[A] =
    M.mix(At[F1].at(path, m1), At[F2].at(path, m2), At[F3].at(path, m3))

  def \(key: String): As3[F1, F2, F3] = As3(path \ key)
  def \(idx: Int): As3[F1, F2, F3] = As3(path \ idx)
  def \(child: PathNode): As3[F1, F2, F3] = As3(path \ child)
}
