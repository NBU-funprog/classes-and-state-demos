package nbu.funprog

import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
  def get: Int
  def put(x: Int)
}

class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get = buf.remove(0)
  def put(x: Int) = buf += x
}

trait Doubling extends IntQueue {
  abstract override def put(x: Int) = super.put(x * 2)
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int) = if (x >= 0) super.put(x)
}

trait Incrementing extends IntQueue {
  abstract override def put(x: Int) = super.put(x + 1)
}

object Queues {
  def main(args: Array[String]) = {
    println("First doubling and then incrementing")
    val q1 = new BasicIntQueue with Incrementing with Doubling
    q1.put(10)
    q1.put(20)
    println(q1.get)
    println(q1.get)
    println()

    println("First incrementing and then doubling")
    val q2 = new BasicIntQueue with Doubling with Incrementing
    q2.put(10)
    q2.put(20)
    println(q2.get)
    println(q2.get)
    println()

    println("First filtering and then incrementing")
    val q3 = new BasicIntQueue with Incrementing with Filtering 
    q3.put(-1)
    q3.put(0)
    q3.put(1)
    println(q3.get)
    println(q3.get)
    println()

    println("First incrementing and then doubling")
    val q4 = new BasicIntQueue with Filtering with Incrementing
    q4.put(-1)
    q4.put(0)
    q4.put(1)
    println(q4.get)
    println(q4.get)
    println()
  }
}
