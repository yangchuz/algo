object AVLTree extends App {

  val myTree = new AVLTree[BigInt]
  for(i <- (1 to 30).reverse){
    val k = (math.random() * 100).toInt
    myTree.add(k)
    //    myTree.add(i)
  }
  println(myTree.height)
  println(myTree.size)
  println(myTree)

}

class AVLTree[E <: Ordered[E]]{
  private var root: Option[Node] = None
  private var _size: Int = 0

  def size: Int = _size
  def height: Int = height(root)

  def add(d: E): Unit = {
    val node = new Node(d)
    root match {
      case None => root = Some(node); _size += 1;
      case Some(r) => add(r, node)
    }
  }

  private def checkBalance(node: Node): Node ={
    if(math.abs(height(node.left) - height(node.right)) > 1) {
      rebalance(node)
    } else {
      node
    }
  }



  private def rebalance(node: Node): Node ={
    val newNode = if(height(node.left) > height(node.right)){
      val l = node.left.get
      if(height(l.left) > height(l.right)){
        rotateRight(node)
      }else{
        rotateLeftRight(node)
      }
    }else{
      val r = node.right.get
      if(height(r.right) > height(r.left)){
        rotateLeft(node)
      }else{
        rotateRightLeft(node)
      }
    }
    newNode
  }

  private def rotateLeft(node: Node): Node = {
    val tmp = node.right.get
    node.right = tmp.left
    tmp.left = Some(node)
    tmp
  }

  private def rotateRight(node: Node): Node = {
    val tmp = node.left.get
    node.left = tmp.right
    tmp.right = Some(node)
    tmp
  }

  private def rotateLeftRight(node: Node): Node = {
    node.left = Some(rotateLeft(node.left.get))
    rotateRight(node)
  }

  private def rotateRightLeft(node: Node): Node = {
    node.right = Some(rotateRight(node.right.get))
    rotateLeft(node)
  }

  private def add(parent: Node, child: Node): Unit = {
    if(child.data >= parent.data){
      parent.right match {
        case Some(p) => add(p, child); parent.right = Some(checkBalance(p))
        case None => parent.right = Some(child); _size += 1
      }
    }else{
      parent.left match {
        case Some(p) => add(p, child); parent.left = Some(checkBalance(p))
        case None => parent.left = Some(child); _size += 1
      }
    }
  }

  private def height(node: Option[Node]): Int ={
    node match {
      case None => -1
      case Some(n) => math.max(height(n.left), height(n.right)) + 1
    }
  }

  override def toString: String = {
    treeToStr(root)
  }

  private def treeToStr(node: Option[Node]):String = {
    node match {
      case None => ""
      case Some(n) => treeToStr(n.left) + n + treeToStr(n.right)
    }
  }

  class Node(val data: E){
    var left: Option[Node] = None
    var right: Option[Node] = None

    override def toString: String = data + ", "
  }
}