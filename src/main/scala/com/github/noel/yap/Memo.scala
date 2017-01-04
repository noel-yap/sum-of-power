package com.github.noel.yap

/**
  * From https://stackoverflow.com/questions/25129721/scala-memoization-how-does-this-scala-memo-work.
  *
  * Generic way to create memoized functions (even recursive and multiple-arg ones)
  *
  * @param f the function to memoize
  * @tparam I input to f
  * @tparam K the keys we should use in cache instead of I
  * @tparam O output of f
  */
case class Memo[I, K, O](f: I => O)(implicit g: I => K) extends (I => O) {
  import scala.collection.mutable.{Map => Dict}

  type Input = I
  type Key = K
  type Output = O

  val cache: Dict[K, O] = Dict.empty[K, O]

  override def apply(x: I): O = cache getOrElseUpdate(x, f(x))
}

object Memo {
  /**
    * Type of a simple memoized function e.g. when I = K
    */
  type ==>[I, O] = Memo[I, I, O]
}

