package com.example.part2
import cats._
import cats.implicits._
import cats.data._
object WriterMonad extends App {

  object Tracked {
    type Tracked[A] = Writer[List[String], A]

    implicit def trackedShow[A: Show]: Show[Tracked[A]] = Show.show { ta =>
      //val logAndVal: (List[String], A) = ta.run
      val (log: List[String], a: A) = ta.run
      (log ++ List(a.show)).mkString("\n")
    }
  }

  import Tracked._
  case class Client(id: Long, name: String, age: Int)

  object Client {
    def makeRaw(id: Long, name: String, age: Int): Tracked[Client] = {
      Client(id, name, age).writer(List(s"Creating client with name: $name"))
      //List("Creating client ...").tell.map(_ => Client(id, name, age))
      //Writer(List("Creating client ..."), Client(id, name, age))
    }
  }
  case class Product(id: Long, name: String, unitPrice: Double)

  object Product {
    def makeRaw(id: Long, name: String, unitPrice: Double): Tracked[Product] =
      Product(id, name, unitPrice).writer(List(s"Creating product with name $name"))
  }
  case class ShoppingCartItem(quantity: Int, product: Product) {
    def total: Double = quantity * product.unitPrice
  }

  object ShoppingCartItem {
    implicit val shoppingCartItemShow: Show[ShoppingCartItem] =
      Show.show(item => s"${item.quantity} x ${item.product.name}")

    def makeRaw(quantity: Int, productId: Long, productName: String, productUnitPrice: Double): Tracked[ShoppingCartItem] =
      for {
        _ <- List("Creating shopping cart item").tell
        product <- Product.makeRaw(productId, productName, productUnitPrice)
      } yield ShoppingCartItem(quantity, product)

  }
  case class ShoppingCart(client: Client, items: List[ShoppingCartItem]) {
    def total: Double = items.map(_.total).sum
  }

  object ShoppingCart {
    implicit val scShow: Show[ShoppingCart] = Show.fromToString

    def makeRaw0(
                 clientId: Long,
                 clientName: String,
                 clientAge: Int,
                 items: List[(Int, Long, String, Double)]): Tracked[ShoppingCart] = {
      val cartItems: Seq[Tracked[ShoppingCartItem]] = items map { case (quantity, productId, productName, productUnitPrice) =>
        ShoppingCartItem.makeRaw(quantity, productId, productName, productUnitPrice)
      }
      for {
        _ <- List("Creating shopping cart").tell
        client <- Client.makeRaw(clientId, clientName, clientAge)
        itemSeq <- cartItems.sequence
        cart = ShoppingCart(client, itemSeq.toList)
      } yield cart
    }

    def makeRaw1(
                 clientId: Long,
                 clientName: String,
                 clientAge: Int,
                 items: List[(Int, Long, String, Double)]): Tracked[ShoppingCart] = {
      val cartMessage: Writer[List[String], Unit] = List("Creating shopping cart item").tell
      val cartItems: Tracked[List[ShoppingCartItem]] = items.traverse { case (quantity, productId, productName, unitPrice) => ShoppingCartItem.makeRaw(quantity, productId, productName, unitPrice)}
      val trackedClient: Tracked[Client] = Client.makeRaw(clientId, clientName, clientAge)
      val mapFun: Tracked[List[ShoppingCartItem] => Client => ShoppingCart] = Writer(List[String](), (itms: List[ShoppingCartItem]) => (clnt: Client) => ShoppingCart(clnt, itms))
      //cartMessage.ap()trackedClient.ap(cartItems.ap(mapFun))
      ???
    }

    def makeRaw2(
                 clientId: Long,
                 clientName: String,
                 clientAge: Int,
                 items: List[(Int, Long, String, Double)]): Tracked[ShoppingCart] = {
      val cartMessage: Writer[List[String], Unit] = List("Creating shopping cart item").tell
      val cartItems = items.traverse { case (quantity, productId, productName, unitPrice) => ShoppingCartItem.makeRaw(quantity, productId, productName, unitPrice) }
      val trackedClient = Client.makeRaw(clientId, clientName, clientAge)
      (cartMessage, cartItems, trackedClient).mapN((_, itms, clnt) => ShoppingCart(clnt, itms))
    }
  }

  trait Discount {
    val name: String
    def applies(client: Client, item: ShoppingCartItem): Boolean
    def amountToDiscount(item: ShoppingCartItem): Double
    def calculateDiscount(client: Client, item: ShoppingCartItem): Tracked[Double] =
      if(applies(client, item)) {
        amountToDiscount(item).writer(List(s"Applied $name"))
      }else 0d.pure[Tracked]
  }

  object Discount {
    object MoreThanFiveUnitsDiscount extends Discount {
      override val name: String = "10% discount for more than 5 items"

      override def applies(client: Client, item: ShoppingCartItem): Boolean = item.quantity > 5

      override def amountToDiscount(item: ShoppingCartItem): Double = item.total * 0.1
    }

    object ElderlyDiscount extends Discount {
      override val name: String = "20% discount for elderly people"

      override def applies(client: Client, item: ShoppingCartItem): Boolean = client.age > 65

      override def amountToDiscount(item: ShoppingCartItem): Double = item.total * 0.2
    }

    val allDiscounts: List[Discount] = List(MoreThanFiveUnitsDiscount, ElderlyDiscount)
  }


  def calculateTotalDiscount0(cart: ShoppingCart, discounts: List[Discount]): Tracked[Double] = {
  //Using applicative to apply every discount to every item!! Since List is an applicative
    val tracked: Seq[Tracked[Double]] = (cart.items, discounts).mapN {
      (item, discount) => discount.calculateDiscount(cart.client, item)
    }
    val trackedData: Tracked[Seq[Double]] = tracked.sequence
    trackedData.map(_.sum)
  }

  def calculateTotalDiscount(cart: ShoppingCart, discounts: List[Discount]): Tracked[Double] =
    //Using applicative to apply every discount to every item!! Since List is an applicative
    /*(cart.items, discounts).mapN {
      (item, discount) => discount.calculateDiscountForClient(cart.client, item)
    }.combineAll *///Uses Monoid instance of Tracked(i.e Writer[List[T]]) and Double
    (cart.items, discounts)
      .tupled
      //as List is Foldable and has Monoid instance, we can fold over it while applying mapping function and combining using the implicit monoid
      .foldMap { case (i, d) => d.calculateDiscount(cart.client, i) }
      //Traverse require an applicative, as Tracked is a Writer monad, which is an Applicative, hence we can traverse.
      //Remember traverse reverse the types, so List of tuple -> Tracked[List[Double]]
      //.traverse { case (i, d) => d.calculateDiscount(cart.client, i) }
      //.map(_.sum)

  def calculateTotal(shoppingCart: ShoppingCart): Tracked[Double] = {
    calculateTotalDiscount(shoppingCart, Discount.allDiscounts)
      .map(a => shoppingCart.total - a)
  }

  val client = Client(1, "leandro", 70)
  val milk = Product(1, "milk", 15.0)
  val eggs = Product(1, "eggs", 25.0)
  val items = List(
    ShoppingCartItem(15, milk),
    ShoppingCartItem(30, eggs)
  )
  val shoppingCart = ShoppingCart(client, items)
  println(calculateTotalDiscount(shoppingCart, Discount.allDiscounts).show)
  println(Show[Tracked[Double]].show(calculateTotal(shoppingCart)))
  val cart = ShoppingCart.makeRaw0(client.id,
    client.name,
    client.age,
    List((1, 2, "eggs", 30), (1, 3, "milk", 15))
  )
  println("#################################################################")
  println(Show[Tracked[ShoppingCart]].show(cart))
}
