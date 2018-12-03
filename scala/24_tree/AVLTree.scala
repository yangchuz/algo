object AVLTree extends App {

  val myTree = new AVLTree[BigInt]
  for(i <- 1 to 10){
    myTree.add((math.random() * 100).toInt)
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
      case None => root = Some(node)
      case Some(r) => add(r, node)
    }

    checkBalance(node)
  }

  private def checkBalance(node: Node): Unit ={
    if(math.abs(height(node.left) - height(node.right)) > 1) {
      rebalance(node)
    }

    node.parent match {
      case None =>
      case Some(p) => checkBalance(p)
    }
  }



  private def rebalance(node: Node): Unit ={
    val parent = node.parent
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

    parent match {
      case None => root = Some(newNode)
      case Some(p) if isLeftChild(p, node) => p.left = Some(newNode)
      case Some(p) if isRightChild(p, node) => p.right = Some(newNode)
    }
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


  private def isLeftChild(parent: Node, child: Node): Boolean = {
    parent.left match {
      case Some(l) if l.data == child.data => true
      case _ => false
    }
  }


  private def isRightChild(parent: Node, child: Node): Boolean = {
    parent.right match {
      case Some(r) if r.data == child.data => true
      case _ => false
    }
  }

  private def add(parent: Node, child: Node): Unit ={
    if(child.data >= parent.data){
      val r = parent.right
      r match {
        case Some(p) => add(p, child)
        case None => parent.right = Some(child); _size += 1
      }
    }else{
      val l = parent.left
      l match {
        case Some(p) => add(p, child)
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
    var parent: Option[Node] = None
    var left: Option[Node] = None
    var right: Option[Node] = None

    override def toString: String = data + ", "
  }
}