package OrderTaking.Common

import OrderTaking.Common.SimpleTypes.{
  EmailAddress,
  String50,
  UsStateCode,
  VipStatus,
  ZipCode
}

/**
  * Common compound types used throughout the OrderTaking domain
  *
  * Includes: customers, addresses, etc.
  * Plus common errors.
  */
object CompoundTypes {

  // ==================================
  // Customer-related types
  // ==================================

  case class PersonalName(firstName: String50, lastName: String50)

  case class CustomerInfo(name: PersonalName,
                          emailAddress: EmailAddress,
                          vipStatus: VipStatus)

  // ==================================
  // Address-related
  // ==================================

  case class Address(addressLine1: String50,
                     addressLine2: Option[String50],
                     addressLine3: Option[String50],
                     addressLine4: Option[String50],
                     city: String50,
                     zipCode: ZipCode,
                     state: UsStateCode,
                     country: String50)

  // ==================================
  // Product-related types
  // ==================================

  // Note that the definition of a Product is in a different bounded
  // context, and in this context, products are only represented by a ProductCode
  // (see the SimpleTypes module).

}
