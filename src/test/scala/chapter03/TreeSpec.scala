package chapter03

import chapter03.Tree._
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  behavior of "treeSize"

  it should "evaluate to correct number of nodes" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    treeSize(tree) shouldBe 5
  }

  behavior of "maximum"

  it should "evaluate to correct maximum element in a Tree" in {
    val tree = Branch(Branch(Leaf(2), Leaf(5)), Branch(Leaf(1), Branch(Leaf(4), Leaf(0))))
    maximum(tree) shouldBe 5
  }

  behavior of "depth"

  it should "evaluate to correct depth of a Tree" in {
    val tree = Branch(Branch(Leaf(2), Leaf(5)), Branch(Leaf(1), Branch(Leaf(4), Leaf(0))))
    depth(tree) shouldBe 3
  }

  behavior of "map"

  it should "evaluate to correct number of nodes" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    map(tree)(_ * 2) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
  }

  behavior of "treeSizeFold"

  it should "evaluate to correct number of nodes via fold" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    treeSizeFold(tree) shouldBe 5
  }

  behavior of "maximumFold"

  it should "evaluate to correct maximum element in a Tree via fold" in {
    val tree = Branch(Branch(Leaf(2), Leaf(5)), Branch(Leaf(1), Branch(Leaf(4), Leaf(0))))
    maximumFold(tree) shouldBe 5
  }

  behavior of "depthFold"

  it should "evaluate to correct depth of a Tree via fold" in {
    val tree = Branch(Branch(Leaf(2), Leaf(5)), Branch(Leaf(1), Branch(Leaf(4), Leaf(0))))
    depthFold(tree) shouldBe 3
  }

  behavior of "mapFold"

  it should "evaluate to correct number of nodes via fold" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    mapFold(tree)(_ * 2) shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
  }
}
