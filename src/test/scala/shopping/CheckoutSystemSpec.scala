package shopping

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CheckoutSystemSpec extends AnyWordSpec
  with Matchers {

  "CheckoutSystem" when {
    "scanItems() is called" should {
      "return valid Item types for a valid basket" in {
        val selectedItems = Seq("apple", "orange", "apple", "grape")
        val expectedItems = Seq(Apple, Orange, Apple, AnyOtherItem)

        CheckoutSystem.scanItems(selectedItems) shouldBe expectedItems
      }

      "return no Items for empty basket" in {
        val selectedItems = Seq.empty
        val expectedItems = Seq.empty

        CheckoutSystem.scanItems(selectedItems) shouldBe expectedItems
      }
    }

    "isValid() is called" should {
      "return true for valid Item" in {
        CheckoutSystem.isValid(Apple) shouldBe true
        CheckoutSystem.isValid(Orange) shouldBe true
      }

      "return false for invalid Item" in {
        CheckoutSystem.isValid(AnyOtherItem) shouldBe false
      }
    }

    "actualPrice() is called" should {
      "return valid price" in {
        CheckoutSystem.actualPrice(Seq.empty) shouldBe 0.0
        CheckoutSystem.actualPrice(Seq(Apple)) shouldBe 0.60
        CheckoutSystem.actualPrice(Seq(Orange)) shouldBe 0.25
        CheckoutSystem.actualPrice(Seq(Apple, Orange, Apple, Apple)) shouldBe 2.05
      }
    }

    "discountPrice() is called" should {
      "return valid discount price" in {
        val discounts = Seq(ItemDiscounts(Apple, 2), ItemDiscounts(Orange, 3))
        CheckoutSystem.discountPrice(Seq.empty, Nil) shouldBe 0.0
        // No discount should be applied because basket doesn't have enough items
        CheckoutSystem.discountPrice(Seq(Apple), discounts) shouldBe 0.0
        CheckoutSystem.discountPrice(Seq(Orange), discounts) shouldBe 0.0
        // Discount should be applied
        CheckoutSystem.discountPrice(Seq(Apple, Orange, Apple), discounts) shouldBe 0.60
        CheckoutSystem.discountPrice(Seq(Apple, Orange, Apple, Orange, Orange), discounts) shouldBe 0.85
      }
    }

    "totalPrice() is called" should {
      "return valid total price" in {

        val discounts = Seq(ItemDiscounts(Apple, 2), ItemDiscounts(Orange, 3))

        // Empty basket
        CheckoutSystem.totalPrice(Seq.empty, Nil) shouldBe 0.0

        // No discount should be applied because basket doesn't have enough items
        CheckoutSystem.totalPrice(Seq("apple"), discounts) shouldBe 0.60
        CheckoutSystem.totalPrice(Seq("orange"), discounts) shouldBe 0.25

        // Discount should be applied and subtract from the actual price
        CheckoutSystem.totalPrice(Seq("apple", "orange", "apple", "apple"), discounts) shouldBe 1.45
        CheckoutSystem.totalPrice(Seq("apple", "orange", "apple", "grape","orange", "orange"), discounts) shouldBe 1.10
      }
    }
  }
}
