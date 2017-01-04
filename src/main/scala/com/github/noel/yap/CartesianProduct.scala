package com.github.noel.yap

class CartesianProduct(val product: Traversable[Traversable[_ <: Any]]) {
  override def toString: String = {
    product.toString
  }

  def *(rhs: Traversable[_ <: Any]): CartesianProduct = {
    val p = product.flatMap { lhs =>
      rhs.map { r =>
        lhs.toList :+ r
      }
    }

    new CartesianProduct(p)
  }
}

object CartesianProduct {
  def apply(traversable: Traversable[_ <: Any]): CartesianProduct = {
    new CartesianProduct(
      traversable.map { t =>
        Traversable(t)
      }
    )
  }
}

