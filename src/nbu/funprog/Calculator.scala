package nbu.funprog

trait Button
case class Value(n: Int) extends Button
case class Sum(op1: Button, op2: Button) extends Button
case class Divide(op1: Button, op2: Button) extends Button
case class Fact(n: Int) extends Button

class Calculator {
  def exec(b: Button): Int = b match {
    case Value(n) => n
    case Sum(op1, op2) => exec(op1) + exec(op2)
    case Divide(op1, op2) => op2 match {
      case Value(0) => 0
      case _ => exec(op1) / exec(op2)
    }
    case Fact(n) => n match {
      case 0 => 1
      case i => n * exec(Fact(n - 1))
    }
  }

  def show: Button => String = {
    case Value(n) => n toString
    case Sum(op1, op2) => show(op1) + " + " + show(op2)
    case Divide(_, Value(0)) => "It's illegal to divide by zero"
    case Divide(op1, op2) => show(op1) + " / " + show(op2)
    case Fact(n) => "!" + n + " = " + (n match {
                                        case 0 | 1 => 1 toString
                                        case i => (1 to i).toList.mkString(" * ")
                                      })
  }

  def bulk(oper: List[Button]) = {
    def iter(oper: List[Button], res: List[Int]): List[Int] = oper match {
      case Nil => res reverse
      case x::xs => iter(xs, exec(x)::res)
    }
    iter(oper, Nil)
  }
}

object Calculator {
  def main(args: Array[String]): Unit = {
    val calc = new Calculator
    import calc._

    val res1 = exec(Value(7))
    println(res1)

    val op1 = Value(9)
    val op2 = Value(-3)
    val op3 = Value(0)
    val res2 = exec(Sum(op1, op2))
    println(res2)

    val res3 = exec(Divide(op1, op2))
    println(res3)

    val res4 = exec(Divide(op1, op3))
    println(res4)

    val res5 = exec(Fact(0))
    println(res5)

    val res6 = exec(Fact(1))
    println(res6)

    val res7 = exec(Fact(5))
    println(res7)

    val res8 = show(Value(-6))
    println(res8)

    val s1 = Value(4)
    val s2 = Value(11)
    val s3 = Value(0)
    val res9 = show(Sum(s1, s2))
    println(res9)

    val res10 = show(Divide(s1, s2))
    println(res10)

    val res11 = show(Divide(s1, s3))
    println(res11)

    val res12 = show(Fact(0))
    println(res12)

    val res13 = show(Fact(1))
    println(res13)

    val res14 = show(Fact(5))
    println(res14)

    val res15 = bulk(Nil)
    println(res15)

    val oper1 = List(Value(-3))
    val res16 = bulk(oper1)
    println(res16)

    val oper2 = List(Value(4), Fact(3), Sum(Value(1), Value(-2)))
    val res17 = bulk(oper2)
    println(res17)
  }
}