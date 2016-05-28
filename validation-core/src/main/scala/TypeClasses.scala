package jto.validation

trait Mixer2[F1[_], F2[_]] {
  def mix[A](m1: => F1[A], m2: => F2[A]): F1[A] with F2[A]
}

object Mixer2 {
  implicit def mixRuleWrite[I, J]: Mixer2[Rule[I, ?], Write[?, J]] =
    new Mixer2[Rule[I, ?], Write[?, J]] {
      def mix[A](m1: => Rule[I, A], m2: => Write[A, J]): Rule[I, A] with Write[A, J] =
        new Rule[I, A] with Write[A, J] {
          def validate(data: I): VA[A] = m1.validate(data)
          def writes(i: A): J = m2.writes(i)
        }
    }
}

trait Mixer3[F1[_], F2[_], F3[_]] {
  def mix[A](m1: => F1[A], m2: => F2[A], m3: => F3[A]): F1[A] with F2[A] with F3[A]
}

@simulacrum.typeclass
trait At[F[_]] {
  def at[A](path: Path, f: F[A]): F[A]
}
