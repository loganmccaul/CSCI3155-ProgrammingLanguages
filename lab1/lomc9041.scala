object Lab1 extends jsy.util.JsyApplication {
  import jsy.lab1.ast._
  import jsy.lab1.Parser
  
  /*
   * CSCI 3155: Lab 1
   * Logan McCaul
   * 
   * Partner: Mitch Zinser
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /*
   * Example with a Unit Test
   * 
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter.
   * 
   * To run a selection in the interpreter in Eclipse, highlight the code of interest
   * and type Ctrl+Shift+X (on Windows) or Cmd+Shift+X (on Mac).
   * 
   * Highlight the next few lines below to try it out.  The assertion passes, so
   * it appears that nothing happens.  You can uncomment the "bad test specification"
   * and see that a failed assert throws an exception.
   * 
   * You can try calling 'plus' with some arguments, for example, plus(1,2).  You
   * should get a result something like 'res0: Int = 3'.
   * 
   * As an alternative, the testPlus2 function takes an argument that has the form
   * of a plus function, so we can try it with different implementations.  For example,
   * uncomment the "testPlus2(badplus)" line, and you will see an assertion failure.
   * 
   * Our convention is that these "test" functions are testing code that are not part
   * of the "production" code.
   * 
   * While writing such testing snippets are convenient, it is not ideal.  For example,
   * the 'testPlus1()' call is run whenever this object is loaded, so in practice,
   * it should probably be deleted for "release".  A more robust way to maintain
   * unit tests is in a separate file.  For us, we use the convention of writing
   * tests in a file called LabXSpec.scala (i.e., Lab1Spec.scala for Lab 1).
   */
  
  def plus(x: Int, y: Int): Int = x + y
  def testPlus1() {
    assert(plus(1,1) == 2)
    //assert(plus(1,1) == 3) // bad test specification
  }
  testPlus1()

  def badplus(x: Int, y: Int): Int = x - y
  def testPlus2(plus: (Int, Int) => Int) {
    assert(plus(1,1) == 2)
  }
  //testPlus2(badplus)

  /* Exercises */

  def abs(n: Double): Double = {
    if (n < 0){
      return (n * -1)
    }
    else{
      return n
    }
  }

  def xor(a: Boolean, b: Boolean): Boolean = {
    if (a){
      //If both a and b are true
      if (b){
        return false
      }
      //Else return true
      else{return true}
    }
    else{
      // if a is false and b if true
      if (b){
        return true
      }
      //Else return false
      else{return false}
    }
  }

  def repeat(s: String, n: Int): String = {
    //Check for negative input length
    if (n < 0){throw new IllegalArgumentException}
    else if (n == 0){return ""}
    else if (n == 1){return s}
    else{return (s + repeat(s, (n-1)))}
  }
  
  def sqrtStep(c: Double, xn: Double): Double = {return xn-((xn*xn-c)/(2*xn))}

  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    if (n < 0){throw new IllegalArgumentException}
    else if (n == 0){return x0}
    else{return sqrtN(c, sqrtStep(c, x0), (n-1))}
  }

  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    require(epsilon > 0)
    //Run next iteration
    var step : Double = sqrtStep(c, x0)
    //Check if difference is within epsilon
    if (abs((step * step - c)) < epsilon){
      return step
    }
    else{return sqrtErr(c, step, epsilon)}
  }

  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }
  
  /* Search Tree */
  
  sealed abstract class SearchTree
  case object Empty extends SearchTree
  case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree
  
  def repOk(t: SearchTree): Boolean = {
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) => {
        //Check that node min is less than parent and that max is more than parent
        if ((min <= d) && (max >= d)){
          //Check both children nodes recursively
          return (check(l, min, d) && check(r, d, max))
        }
        //Otherwise return false
        else{return false}
      }
    }
    check(t, Int.MinValue, Int.MaxValue)
  }
  
  def insert(t: SearchTree, n: Int): SearchTree = t match{
    //If tree is empty, just create new node with no childeren and value n
    case Empty => new Node(Empty, n, Empty)
    //If tree isn't empty
    case Node(l, d, r) => {

      //If value is less than value of parent node, insert on left by calling insert again
      if (n < d){
        return new Node(insert(l,n), d, r)
      }
      //If value is greater or equal, add to right
      else{
          return new Node(l, d, insert(r,n))
      }
    }
  }
  
  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) => {
        //l1 is the new search tree, m is the number that was deleted
        val (l1, m) = deleteMin(l)
        //return results of recursive call to the left node until smallest value is reached
        return (Node(l1, d, r), m)
      }
    }
  }
 
  def delete(t: SearchTree, n: Int): SearchTree = t match {
    //If node is empty, then do nothing and return Empty
    case Empty => return Empty
    //Else, begin deleting
    case Node(l, d, r) => {
      //If n is greater than this node, return new node calling delete on right child
      if (n > d) {return new Node(l, d, delete(r, n))}
      //If n is greater than this node, return new node calling delete on left child
      else if (n < d) {return new Node(delete(l, n), d, r)}
      //If n is the number we want, delete node and update tree
      else {
        //If no left child
        if (l == Empty){
          //If right child is empty also, no more updating required
          if (r == Empty) {return Empty}
          //Otherwise update with right child
          else {return r}
        }
        //If left child isn't empty, but right child is, return left child
        else if (r == Empty) {return l}
        //If both nodes are full
        else{
          //l1 is the updated tree, d1 is the updated value from the deleted right ndoe
          val(l1, d1) = deleteMin(r)
          return Node(l, d1, l1)
        }
      }
    }
  }
  
  /* JavaScripty */
  
  def eval(e: Expr): Double = e match {
    case N(n) => return n
    //Switch to negative
    case Unary(neg, en) => return -1*eval(en)
    //Binary addition
    case Binary(Plus, en, ed) => {
      return eval(en) + eval(ed)
    }
    //Binary subtraction
    case Binary(Minus, en, ed) => {
      return eval(en) - eval(ed)
    }
    //Binary multiplication
    case Binary(Times, en, ed) => {
      return eval(en) * eval(ed)
    }
    //Binary divide
    case Binary(Div, en, ed) => {
      //Checks if the denominator is 0
      if (eval(ed) == 0) {
        //Check numerator, if it is positive, return positive Infinity, else return negative Infinity
        if (eval(en) > 0) {return Double.PositiveInfinity}
        else {return Double.NegativeInfinity}
      }
      //Else return the result of the division
      else {return (eval(en)/eval(ed))}
    }
    case _ => throw new UnsupportedOperationException
  }
  
 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */ 
  
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(v)
  }

}
