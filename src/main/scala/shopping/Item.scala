package shopping

sealed trait Item {
  val price: BigDecimal
}

case class ItemDiscounts(item: Item, discount: Int)

case object Apple extends Item {
  override val price: BigDecimal = 0.6
}

case object Orange extends Item {
  override val price: BigDecimal = 0.25
}

case object AnyOtherItem extends Item {
  override val price: BigDecimal = 0.0
}
