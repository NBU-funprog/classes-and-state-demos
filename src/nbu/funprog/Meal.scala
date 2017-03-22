package nbu.funprog

sealed abstract class Food

case class Fish(name: String) extends Food
case class Fruit(name: String) extends Food
case class Game(name: String, weight: Int) extends Food

object Meal {
  def main(args: Array[String]) = {
    println(order(Fish("mackerel")))
    println(order(Fish("salmon")))
    println(order(Fruit("strawberry")))
    println(order(Fruit("kiwi")))
    println(order(Game("boar", 68)))
    println(order(Game("boar", 34)))
    println(order(Game("deer", 24)))
  }

  def order(f: Food) = f match {
    case Fish(n) if n equals("mackerel") => "Order mackarel" // constructor + variable model + pattern guard
    case Fish(_) => "Order whatever fish we have in stock" // constructor + wildcard model
    case Fruit("strawberry") => "Order strawberries" // constructor + constant model
    case n: Fruit => "Order some fresh fruits" // type model
    case Game(n, w) if (n equals "boar") && (w > 50) => "Order a boar which is at least 50 kilos" // constructor + variable + pattern guard model
    case Game(_, _) => "Order whatever game we have" // constructor + wildcard model
  }
}
