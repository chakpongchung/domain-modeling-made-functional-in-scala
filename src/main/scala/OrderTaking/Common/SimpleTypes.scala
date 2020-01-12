package OrderTaking.Common

import scala.util.matching.Regex

//import scala.language.implicitConversions
//import scala.language.reflectiveCalls

object SimpleTypes {

//  implicit def Pipeline[T](x: T): Object {
//    def |>[S](f: T => S): S
//  } = new {
//    def |>[S](f: T => S): S = f(x)
//  }

  // ===============================
  // Simple types and constrained types related to the OrderTaking domain.
  // ===============================

  /** Constrained to be 50 chars or less, not null */
  case class String50 private (value: String)

  /** An email address */
  case class EmailAddress private (value: String)

  /** Customer's VIP status */
  sealed trait VipStatus
  case object Normal extends VipStatus
  case object Vip extends VipStatus

  /** A zip code */
  case class ZipCode private (value: String)

  /** A US 2 letter state code */
  case class UsStateCode private (value: String)

  /** An Id for Orders. Constrained to be a non-empty string < 10 chars */
  case class OrderId private (value: String)

  /** An Id for OrderLines. Constrained to be a non-empty string < 10 chars */
  case class OrderLineId private (value: String)

  /** The codes for Widgets start with a "W" and then four digits */
  case class WidgetCode private (value: String)

  /** The codes for Gizmos start with a "G" and then three digits. */
  case class GizmoCode private (value: String)

  /** A ProductCode is either a Widget or a Gizmo */
  sealed trait ProductCode
  final case class Widget(value: WidgetCode) extends ProductCode
  final case class Gizmo(value: GizmoCode) extends ProductCode

  /** Constrained to be a integer between 1 and 1000 */
  case class UnitQuantity private (value: Int)

  /** Constrained to be a decimal between 0.05 and 100.00 */
  case class KilogramQuantity private (value: Double)

  /** A Quantity is either a Unit or a Kilogram */
  sealed trait OrderQuantity
  final case class Unit(value: UnitQuantity) extends OrderQuantity
  final case class Kilogram(value: KilogramQuantity) extends OrderQuantity

  /** Constrained to be a decimal between 0.0 and 1000.00 */
  case class Price private (value: Double)

  /** Constrained to be a decimal between 0.0 and 10000.00 */
  case class BillingAmount private (value: Double)

  /** Represents a PDF attachment */
  case class PdfAttachment(name: String, bytes: Array[Byte])

  case class PromotionCode private (value: String)

  // ===============================
  // Reusable constructors and getters for constrained types
  // ===============================

  /**
    * Useful functions for constrained types
    */
  object ConstrainedType {

    /**
      * Create a constrained string using the constructor provided
      * Return Error if input is empty, or length > maxLen
      */
    def createString[S](fieldName: String,
                        ctor: String => S,
                        maxLen: Int,
                        str: String): Either[String, S] = {
      if (str.isEmpty) {
        val msg = s"$fieldName must not be empty"
        Left(msg)
      } else if (str.length > maxLen) {
        val msg = s"$fieldName must not be more than $maxLen chars"
        Left(msg)
      } else {
        Right(ctor(fieldName))
      }
    }

    /**
      * Create a optional constrained string using the constructor provided
      * Return None if input is empty.
      * Return error if length > maxLen
      * Return Some if the input is valid
      */
    def createStringOption[S](fieldName: String,
                              ctor: String => S,
                              maxLen: Int,
                              str: String): Either[String, Option[S]] = {
      if (str.isEmpty) {
        Right(None)
      } else if (str.length > maxLen) {
        val msg = s"$fieldName must not be more than $maxLen chars"
        Left(msg)
      } else {
        Right(Some(ctor(fieldName)))
      }
    }

    /**
      * Create a constrained integer using the constructor provided
      * Return Error if input is less than minVal or more than maxVal
      */
    def createInt[S](fieldName: String,
                     ctor: Int => S,
                     minVal: Int,
                     maxVal: Int,
                     i: Int): Either[String, S] = {
      if (i < minVal) {
        val msg = s"$fieldName: Must not be less than $minVal"
        Left(msg)
      } else if (i > maxVal) {
        val msg = s"$fieldName: Must not be greater than $maxVal"
        Left(msg)
      } else {
        Right(ctor(i))
      }
    }

    /**
      * Create a constrained decimal using the constructor provided
      * Return Error if input is less than minVal or more than maxVal
      */
    def createDecimal[S](fieldName: String,
                         ctor: Double => S,
                         minVal: Double,
                         maxVal: Double,
                         i: Double): Either[String, S] = {
      if (i < minVal) {
        val msg = s"$fieldName: Must not be less than $minVal"
        Left(msg)
      } else if (i > maxVal) {
        val msg = s"$fieldName: Must not be greater than $maxVal"
        Left(msg)
      } else {
        Right(ctor(i))
      }
    }

    /**
      * Create a constrained string using the constructor provided
      * Return Error if input is empty, or does not match the regex pattern
      */
    def createLike[S](fieldName: String,
                      ctor: String => S,
                      pattern: Regex,
                      str: String): Either[String, S] =
      str match {
        case ""           => Left(s"$fieldName: Must not be empty")
        case pattern(str) => Right(ctor(str))
        case _            => Left(s"$fieldName: '$str' must match the pattern '$pattern'")
      }

  }

  object String50 {

    /**
      * Create an String50 from a string
      * Return Error if input is empty, or length > 50
      */
    def create(fieldName: String, str: String): Either[String, String50] =
      ConstrainedType.createString(fieldName, String50.apply, 50, str)

    /**
      * Create an String50 from a string
      * Return None if input is empty.
      * Return error if length > maxLen
      * Return Some if the input is valid
      */
    def createOption(fieldName: String,
                     str: String): Either[String, Option[String50]] =
      ConstrainedType.createStringOption(fieldName, String50.apply, 50, str)

    private[this] def apply(value: String) = new String50(value)

  }

  object EmailAddress {

    private val EmailAddressPattern: Regex = "(.+@.+)".r

    /**
      * Create an EmailAddress from a string
      * Return Error if input is empty, or doesn't have an "@" in it
      */
    def create(fieldName: String, str: String): Either[String, EmailAddress] =
      ConstrainedType.createLike(fieldName,
                                 EmailAddress.apply,
                                 EmailAddressPattern,
                                 str)

    private[this] def apply(value: String) = new EmailAddress(value)

  }

  object VipStatus {

    /**
      * Return a string representation of VipStatus
      */
    def value(status: VipStatus): String = status match {
      case Normal => "Normal"
      case Vip    => "VIP"
    }

    /**
      * Create a VipStatus from a string
      * Return Error if input is null, empty, or doesn't match one of the cases
      */
    def create(fieldName: String, str: String): Either[String, VipStatus] =
      str match {
        case "normal" | "Normal" => Right(Normal)
        case "vip" | "VIP"       => Right(Vip)
        case _                   => Left(s"$fieldName: Must be one of 'Normal', 'VIP'")
      }

  }

  object ZipCode {

    private val ZipCodePattern: Regex = "(\\d{5})".r

    /**
      * Create a ZipCode from a string
      * Return Error if input is null, empty, or doesn't have 5 digits
      */
    def create(fieldName: String, str: String): Either[String, ZipCode] =
      ConstrainedType.createLike(fieldName, ZipCode.apply, ZipCodePattern, str)

    private[this] def apply(value: String) = new ZipCode(value)

  }

  object UsStateCode {

    private val UsStateCodePattern: Regex =
      "(^(A[KLRZ]|C[AOT]|D[CE]|FL|GA|HI|I[ADLN]|K[SY]|LA|M[ADEINOST]|N[CDEHJMVY]|O[HKR]|P[AR]|RI|S[CD]|T[NX]|UT|V[AIT]|W[AIVY])$)".r

    /**
      * Create a UsStateCode from a string
      * Return Error if input is null, empty, or doesn't have 2 letters
      */
    def create(fieldName: String, str: String): Either[String, UsStateCode] =
      ConstrainedType.createLike(fieldName,
                                 UsStateCode.apply,
                                 UsStateCodePattern,
                                 str)

    private[this] def apply(value: String) = new UsStateCode(value)

  }

  object OrderId {

    /**
      * Create an OrderId from a string
      * Return Error if input is null, empty, or length > 50
      */
    def create(fieldName: String, str: String): Either[String, OrderId] =
      ConstrainedType.createString(fieldName, OrderId.apply, 50, str)

    private[this] def apply(value: String) = new OrderId(value)

  }

  object OrderLineId {

    /**
      * Create an OrderLineId from a string
      * Return Error if input is null, empty, or length > 50
      */
    def create(fieldName: String, str: String): Either[String, OrderLineId] =
      ConstrainedType.createString(fieldName, OrderLineId.apply, 50, str)

    private[this] def apply(value: String) = new OrderLineId(value)

  }

  object WidgetCode {

    // The codes for Widgets start with a "W" and then four digits
    private val WidgetCodePattern: Regex =
      "(W\\d{4})".r

    /**
      * Create an WidgetCode from a string
      * Return Error if input is null. empty, or not matching pattern
      */
    def create(fieldName: String, str: String): Either[String, WidgetCode] =
      ConstrainedType.createLike(fieldName,
                                 WidgetCode.apply,
                                 WidgetCodePattern,
                                 str)

    private[this] def apply(value: String) = new WidgetCode(value)

  }

  object GizmoCode {

    // The codes for Gizmos start with a "G" and then three digits.
    private val GizmoCodePattern: Regex =
      "(G\\d{3})".r

    /**
      * Create an GizmoCode from a string
      * Return Error if input is null, empty, or not matching pattern
      */
    def create(fieldName: String, str: String): Either[String, GizmoCode] =
      ConstrainedType.createLike(fieldName,
                                 GizmoCode.apply,
                                 GizmoCodePattern,
                                 str)

    private[this] def apply(value: String) = new GizmoCode(value)

  }

  object ProductCode {

    /**
      * Return the string value inside a ProductCode
      */
    def value(productCode: ProductCode): String = productCode match {
      case Widget(WidgetCode(wc)) => wc
      case Gizmo(GizmoCode(gc))   => gc
    }

    /**
      * Create an ProductCode from a string
      * Return Error if input is null, empty, or not matching pattern
      */
    def create(fieldName: String, code: String): Either[String, ProductCode] = {
      if (code.isEmpty) {
        val msg = s"$fieldName: Must not be null or empty"
        Left(msg)
      } else if (code.startsWith("W")) {
        WidgetCode.create(fieldName, code).map(Widget)
      } else if (code.startsWith("G")) {
        GizmoCode.create(fieldName, code).map(Gizmo)
      } else {
        val msg = s"$fieldName: Format not recognized '$code'"
        Left(msg)
      }
    }

  }

  object UnitQuantity {

    /**
      * Create a UnitQuantity from a int
      * Return Error if input is not an integer between 1 and 1000
      */
    def create(fieldName: String, v: Int): Either[String, UnitQuantity] =
      ConstrainedType.createInt(fieldName, UnitQuantity.apply, 1, 1000, v)

    private[this] def apply(value: Int) = new UnitQuantity(value)

  }

  object KilogramQuantity {

    /**
      * Create a KilogramQuantity from a decimal.
      * Return Error if input is not a decimal between 0.05 and 100.00
      */
    def create(fieldName: String, v: Double): Either[String, KilogramQuantity] =
      ConstrainedType.createDecimal(fieldName,
                                    KilogramQuantity.apply,
                                    0.05D,
                                    100.00D,
                                    v)

    private[this] def apply(value: Double) = new KilogramQuantity(value)

  }

  object OrderQuantity {

    /**
      * Return the value inside a OrderQuantity
      */
    def value(qty: OrderQuantity): Double = qty match {
      case Unit(UnitQuantity(uq))         => uq
      case Kilogram(KilogramQuantity(kq)) => kq
    }

    /**
      * Create a OrderQuantity from a productCode and quantity
      */
    def create(fieldName: String,
               productCode: ProductCode,
               quantity: Double): Either[String, OrderQuantity] =
      productCode match {
        case Widget(_) =>
          UnitQuantity.create(fieldName, quantity.toInt).map(Unit)
        case Gizmo(_) =>
          KilogramQuantity.create(fieldName, quantity).map(Kilogram)
      }
  }

  object Price {

    /**
      * Create a Price from a decimal.
      * Return Error if input is not a decimal between 0.0 and 1000.00
      */
    def create(v: Double): Either[String, Price] =
      ConstrainedType.createDecimal("Price", Price.apply, 0.0D, 1000D, v)

    /**
      * Create a Price from a decimal.
      * Throw an exception if out of bounds. This should only be used if you know the value is valid.
      */
    def unsafeCreate(v: Double): Price = create(v) match {
      case Left(err) =>
        throw new Exception(s"Not expecting Price to be out of bounds: $err")
      case Right(price) => price
    }

    /**
      * Multiply a Price by a decimal qty.
      * Return Error if new price is out of bounds.
      */
    def multiply(qty: Double, p: Price): Either[String, Price] =
      create(qty * p.value)
  }

  object BillingAmount {

    /**
      * Create a BillingAmount from a decimal.
      * Return Error if input is not a decimal between 0.0 and 10000.00
      */
    def create(v: Double): Either[String, BillingAmount] =
      ConstrainedType.createDecimal("BillingAmount",
                                    BillingAmount.apply,
                                    0.0D,
                                    10000D,
                                    v)

    /**
      * Sum a list of prices to make a billing amount
      * Return Error if total is out of bounds
      */
    def sumPrices(prices: List[Price]): Either[String, BillingAmount] = {
      val total = prices.map(_.value).sum
      create(total)
    }
  }

}
