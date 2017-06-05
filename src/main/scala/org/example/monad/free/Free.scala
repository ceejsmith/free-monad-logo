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
    override def pure[A](a: A): Id[A] = {
      println("Called pure on Id monad. Returning " + a)
      a
    }
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = {
      println("Called flatMap on Id monad. Flat mapping " + a)
      f(a)
    }
  }

  // Natural transformation definition
  trait ~>[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  // Free monad definition
  sealed abstract class Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => {
        println(s"Called flatMap on Return(${a})")
        f(a)
      }
      case Bind(fx, g) => {
        println(s"Called flatMap on Bind(${fx}, ...)")
        Bind(fx, g andThen (_ flatMap f))
      }
    }

    def map[B](f: A => B): Free[F, B] = {
      println("Called map on free monad")
      flatMap(a => Return(f(a)))
    }

    def foldMap[G[_]: Monad](f: F ~> G): G[A] = this match {
      case Return(a) => {
        println(s"Called foldMap on Return(${a})")
        Monad[G].pure(a)
      }
      case Bind(fx, g) => {
        println(s"Called foldMap on Bind(${fx}, ...)")
        Monad[G].flatMap(f(fx)) { a =>
          g(a).foldMap(f)
        }
      }
    }
  }

  // Free monad algebra
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Bind[F[_], I, A](a: F[I], f: I => Free[F, A]) extends Free[F, A]

  // I guessed this, and it seems to work. I'd like to know why! I thought I was going
  // to have to understand CoYoneda for this, but suspect this is a simple case?
  def liftF[F[_], A](value: F[A]): Free[F, A] = {
    println(s"Lifting ${value} into free monad")
    Bind(value, (i: A) => Return(i))
  }
}