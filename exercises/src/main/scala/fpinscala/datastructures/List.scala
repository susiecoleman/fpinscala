package fpinscala.datastructures

import scala.annotation.tailrec


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => t
    case _ => Nil
  }

  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Cons(h,t) => Cons(newHead, t)
    case _ => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n > 0) {
      l match {
        case Cons(h, t) => drop(t, n-1)
        case _ => Nil
      }
    } else {
      l
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    def dropWhileAcc(list: List[A], acc: List[A]): List[A] = {
      list match {
        case Cons(h,t) => if (f(h)) dropWhileAcc(t, Cons(h, acc)) else acc
        case _ => acc
      }
    }
    dropWhileAcc(l, Nil)
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case _ => Nil
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def foldLeftAcc(l: List[A], acc: B): B = {
      l match {
        case Cons(h, t) => foldLeftAcc(t, f(acc, h))
        case Nil => acc
      }
    }
    foldLeftAcc(as, z)
  }

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]) = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((x: A, y: B) => f(y, x))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((x: B, y: A) => f(y, x))

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((h, list) => Cons(h, list))
  }

  def listConcat[A](lists: List[List[A]]): List[A] = {
    foldLeft(lists, List[A]())((h, l) => append(h, l))
  }

  def addOneLists(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((item, acc) => Cons(item + 1, acc))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, List[String]())((item, acc) => Cons(item.toString, acc))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((item, acc) => Cons(f(item), acc))
}

object Main {

  def main(args: Array[String]): Unit = {
    val l1 = List(1, 4, 7)
    val l2 = List(3,1,2)
    val l3 = List(7,8,2,0)
    val lists = List(l1,l2,l3)
    val l4 = List(3.4, 5.6, 12.1)
    assert(List.append(l1, l2) == List(1,4,7,3,1,2))
    assert(List.tail(l1) == List(4,7))
    assert(List.setHead(l1, 19) == List(19, 4,7))
    assert(List.drop(l1, 0) == List(1, 4,7))
    assert(List.drop(l1, 5) == List())
    assert(List.drop(l1, 2) == List(7))
    assert(List.dropWhile(l1)(_%2 != 0) == List(1))
    assert(List.foldRight(l1, "List")((a,b) => a+b) == "147List")
    assert(List.foldRight2(l1, "List")((a,b) => a+b) == "147List")
    assert(List.foldLeft(l1, "List")((a,b) => a+b) == "List147")
    assert(List.foldLeft2(l1, "List")((a,b) => a+b) == "List147")
    assert(List.length(l1) == 3)
    assert(List.length2(l1) == 3)
    assert(List.reverse(l1) == List(7,4,1))
    assert(List.listConcat(lists) == List(1,4,7,3,1,2,7,8,2,0))
    assert(List.addOneLists(l1) == List(2,5,8))
    assert(List.doubleToString(l4) == List("3.4", "5.6", "12.1"))
    assert(List.map(l1)(_.toString + "**") == List("1**", "4**", "7**"))
  }
}
