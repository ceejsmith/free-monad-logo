package org.example.monad.free

import scala.language.higherKinds

object BareBones {
  // Monad definition
  trait Monad[M[_]] {
    def pure[A](a: A): M[A]
    def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]
  }

  // Natural transformation definition
  trait ~>[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  // Free monad definition
  sealed abstract class FreeB[F[_], A] {
    def flatMap[B](f: A => FreeB[F, B]): FreeB[F, B] = this match {
      case Return(a) => f(a)
      case Bind(fx, g) =>
        Bind(fx, g andThen (_ flatMap f))
    }

    def map[B](f: A => B): FreeB[F, B] = flatMap(a => Return(f(a)))

    def foldMap[G[_]: Monad](f: F ~> G): G[A] = this match {
      case Return(a) => Monad[G].pure(a)
      case Bind(fx, g) => Monad[G].flatMap(f(fx)) { a =>
        g(a).foldMap(f)
      }
    }
  }

  // Free monad algebra
  case class Return[F[_], A](a: A) extends FreeB[F, A]
  case class Bind[F[_], I, A](a: F[I], f: I => FreeB[F, A]) extends FreeB[F, A]
}