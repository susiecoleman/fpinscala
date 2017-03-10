package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def maximum2(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((a,b) =>(a max b) + 1)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

  def fold[A, B](tree: Tree[A])(f: A => B)(combine: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l,r) => combine(fold(l)(f)(combine), fold(r)(f)(combine))
  }

}

object Main2 {
  def main(args: Array[String]): Unit = {
    val t = Leaf(5)
    val t1 = Branch(Leaf(1), Branch(Leaf(4), Leaf(2)))
    val t2 = Branch(Branch(Leaf(7), Branch(Leaf(6), Leaf(1))),Branch(Branch(Leaf(3), Leaf(2)), Leaf(12)))
    assert(Tree.size(t) == 1)
    assert(Tree.size(t1) == 5)
    assert(Tree.size(t2) == 11)

    assert(Tree.size2(t) == 1)
    assert(Tree.size2(t1) == 5)
    assert(Tree.size2(t2) == 11)

    assert(Tree.maximum(t) == 5)
    assert(Tree.maximum(t1) == 4)
    assert(Tree.maximum(t2) == 12)

    assert(Tree.maximum2(t) == 5)
    assert(Tree.maximum2(t1) == 4)
    assert(Tree.maximum2(t2) == 12)

    assert(Tree.depth(t) == 1)
    assert(Tree.depth(t1) == 3)
    assert(Tree.depth(t2) == 4)

    assert(Tree.depth2(t) == 1)
    assert(Tree.depth2(t1) == 3)
    assert(Tree.depth2(t2) == 4)

    assert(Tree.map(t)(_+"8") == Leaf("58"))
    assert(Tree.map(t1)(_+"8") == Branch(Leaf("18"), Branch(Leaf("48"), Leaf("28"))))
    assert(Tree.map(t2)(_+"8") == Branch(Branch(Leaf("78"), Branch(Leaf("68"), Leaf("18"))),Branch(Branch(Leaf("38"), Leaf("28")), Leaf("128"))))

    assert(Tree.map2(t)(_+"8") == Leaf("58"))
    assert(Tree.map2(t1)(_+"8") == Branch(Leaf("18"), Branch(Leaf("48"), Leaf("28"))))
    assert(Tree.map2(t2)(_+"8") == Branch(Branch(Leaf("78"), Branch(Leaf("68"), Leaf("18"))),Branch(Branch(Leaf("38"), Leaf("28")), Leaf("128"))))

  }
}