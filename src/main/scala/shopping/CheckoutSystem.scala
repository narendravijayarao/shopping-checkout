package shopping

/*
  CheckoutSystem to scan, validate and calculate the total price of the basket

  Possibility of having different access modifiers for the functions.
  For testing I made them all public
 */
object CheckoutSystem {

  def scanItems(items: Seq[String]): Seq[Item] = Option(items) match {
    case Some(itemList) =>
      itemList.map {
        case "apple" => Apple
        case "orange" => Orange
        case _ => AnyOtherItem
      }
    case _ => Nil
  }

  def isValid(item: Item): Boolean = item match {
    case Apple => true
    case Orange => true
    case _ => false
  }

  def totalPrice(items: Seq[String], discounts: Seq[ItemDiscounts] = Nil): BigDecimal = discounts match {
    case Nil | null => actualPrice(scanItems(items))
    case _ => actualPrice(scanItems(items)) - discountPrice(scanItems(items), discounts)
  }

  def actualPrice(items: Seq[Item]): BigDecimal = items.filter(isValid).foldLeft(BigDecimal(0))(_ + _.price)

  def discountPrice(items: Seq[Item], discounts: Seq[ItemDiscounts]): BigDecimal =
    discounts.foldLeft(BigDecimal(0))((discountPrice, y) => {
      discountPrice + items.count(_ == y.item) / y.discount * y.item.price})
}
