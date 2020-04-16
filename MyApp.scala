package foo

import scala.annotation.tailrec

case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(cabinCode: String,
                      rateCode: String, price: BigDecimal)
case class BestGroupPrice(cabinCode: String,
                          rateCode: String, price: BigDecimal,
                          rateGroup: String)

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

object MyApp extends App {
  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val rateMap = rates.groupMap(_.rateCode)(_.rateGroup).mapValues(_.head)
    prices.groupMapReduce(cp => (cp.cabinCode, rateMap(cp.rateCode)))(cab => BestGroupPrice(cab.cabinCode, cab.rateCode, cab.price, rateMap(cab.rateCode))) { (a, b) =>
      if (a.price < b.price) a else b
    }.values.toSeq
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    @tailrec
    def combinablePromotions(promotions: Seq[Promotion], map: Map[Seq[String], String] = Map.empty): Map[Seq[String], String] = {
      promotions match {
        case Nil => map
        case h :: t =>
          val available = allPromotions.filter(p => !(h.code +: h.notCombinableWith).contains(p.code))
          val currentCombos = buildMap(h, available.size, available, map)
          combinablePromotions(t, map ++ currentCombos)
      }
    }

    @tailrec
    def buildMap(current: Promotion, index: Int, available: Seq[Promotion], map: Map[Seq[String], String]): Map[Seq[String], String] = {
      index match {
        case 0 => map
        case i => {
          val combo = getCombo(current, available, current.notCombinableWith)
          buildMap(current, i-1, available.tail :+ available.head, map ++ Map(combo.sorted -> current.code))
        }
      }
    }

    @tailrec
    def getCombo(current: Promotion, available: Seq[Promotion], blacklist: Seq[String], combination: Seq[String] = Seq.empty): Seq[String] = {
      available match {
        case Nil => current.code +: combination
        case h :: t if blacklist.contains(h.code) => getCombo(current, t, blacklist, combination)
        case h :: t => getCombo(current, t, blacklist ++ h.notCombinableWith, combination :+ h.code)
      }
    }

    combinablePromotions(allPromotions).keys.map(PromotionCombo).toSeq
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allCombinablePromotions(allPromotions).filter(_.promotionCodes.contains(promotionCode))
  }

  val rates = Seq(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val prices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )

  val promotions = Seq(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")), // P5 is not combinable with P2
  )

  println(getBestGroupPrices(rates, prices))

  println(allCombinablePromotions(promotions))

  println(combinablePromotions("P1", promotions))

  println(combinablePromotions("P3", promotions))
}

