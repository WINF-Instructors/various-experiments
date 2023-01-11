import javax.net.ssl.TrustManager
sealed trait Tree {
  def depth(): Int
  def is_in(element: Int): Boolean
  def is_valid(): Boolean
  def eval(): Int
}

case class Leaf(value: Int) extends Tree {
  def depth(): Int = 0
  def is_in(element: Int): Boolean = value==element
  def is_valid(): Boolean = true
  def eval() = value
  override def toString(): String = value.toString()
}
case class Branch(value: Int, left:Tree, right: Tree) extends Tree {
  def depth(): Int = {
    val l = left.depth()
    val r = right.depth()
    if(l>r) l+1 else r+1
  }
  def is_in(element: Int): Boolean = value==element || left.is_in(element) || right.is_in(element)
  def is_valid(): Boolean = (left.eval()<= value) && (right.eval()>=value) && (left.is_valid()) && (right.is_valid())
  def eval()=value
  override def toString(): String = "(value: " + value + ", left: (" + left + ") | right: (" + right + "))"

}

@main def main: Unit = 
  val tree = Branch(1, Branch(2, Leaf(3),Leaf(4)), Leaf(5))
  println(tree)
  println(tree.depth())
  println(tree.is_in(1))
  println(tree.is_in(5))
  println("valid: " + tree.is_valid())
  val tree2 = Branch(3, Branch(2, Leaf(1), Leaf(2)), Leaf(4))
  println("valid: " + tree2.is_valid())