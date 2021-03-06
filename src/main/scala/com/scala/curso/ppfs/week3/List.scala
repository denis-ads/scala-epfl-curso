package com.scala.curso.ppfs.week3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T] (val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false
}

class Nil[T] extends List[T]{
  override def isEmpty = true

  override def head: Nothing= throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

