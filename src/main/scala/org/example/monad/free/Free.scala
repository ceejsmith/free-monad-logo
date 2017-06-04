package org.example.monad.free

import scala.language.higherKinds

object Free {
  // Monad definition
  trait Monad[M[_]] {
    def pure[A](a: A): M[A]
    def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]
  }

  type Id[A] = A

  implicit val identityMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](a: A): Id[A] = a
    override def flatMap[A, B](a: Id[A])(f: (A) => Id[B]): Id[B] = f(a)
  }

  // Natural transformation definition
  trait ~>[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  // Free monad definition
  sealed abstract class Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case Bind(fx, g) =>
        Bind(fx, g andThen (_ flatMap f))
    }

    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))

    def foldMap[G[_]: Monad](f: F ~> G): G[A] = this match {
      case Return(a) => Monad[G].pure(a)
      case Bind(fx, g) => Monad[G].flatMap(f(fx)) { a =>
        g(a).foldMap(f)
      }
    }
  }

  // Free monad algebra
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Bind[F[_], I, A](a: F[I], f: I => Free[F, A]) extends Free[F, A]

  // This was used with the Cats implementation, which reasons in terms of Pure, Suspend and FlatMapped
  // Does it still make sense with an implementation using Return and Bind?
  def liftF[F[_], A](value: F[A]): Free[F, A] = ???
}