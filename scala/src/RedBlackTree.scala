class RedBlackTree[K, V] extends RedBlacI[K, V] {

  private var root: Option[Node] = None
  private var size: Int = 0





}

abstract class RedBlacI[K, V] {
  class Node(var k: K, var v: V) {
    var left, right, parent: Option[Node] = None
    var isLeftChild, isBlack: Boolean = false
  }
}

