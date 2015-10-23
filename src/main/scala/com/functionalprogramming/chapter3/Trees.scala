package com.functionalprogramming.chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

class Trees {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(e) => e
      case Branch(left, right) => maximum(left).max(maximum(right))
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(e) => Leaf(f(e))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(e) => f(e)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(e => 1)((left, right) => 1 + left + right)
  }

  def maximumViaFold(tree: Tree[Int]): Int = {
    fold(tree)(e => e)((left, right) => left.max(right))
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(e => 1)((left, right) => 1 + left.max(right))
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(e => Leaf(f(e)): Tree[B])((left, right) => Branch(left, right))
  }
}
