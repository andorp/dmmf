module OrderTaking.Domain -- 'where'

-- %access export -- ISSUE???

import Control.App
import Data.DPair
import Data.Nat
import Data.List
import Data.Strings

import Common.Prelude
import Common.App


||| Gizmo Code has a fixed format, starting with G then 4 digits.
public export
data GizmoCode : Type where
  MkGizmoCode : String -> GizmoCode

||| Widget Code has a fixed format, starting with W then 4 digits.
public export
data WidgetCode : Type where
  MkWidgetCode : String -> WidgetCode

namespace ProductCode
  public export
  data ProductCode : Type where
    Widget : WidgetCode -> ProductCode
    Gizmo  : GizmoCode  -> ProductCode

  public export
  data ProductCodeError : Type where
    MkProductCodeError : String -> ProductCodeError

  export
  create : String -> Result ProductCode ProductCodeError
  create s = if ("W" `isPrefixOf` s) then Ok $ Widget $ MkWidgetCode s
        else if ("G" `isPrefixOf` s) then Ok $ Gizmo  $ MkGizmoCode  s
        else Error $ MkProductCodeError $ "Unknown Product Code: " ++ s

  -- TODO: Handle eliminator better!
  public export
  productCode : (widget : WidgetCode -> a) -> (gizmo : GizmoCode -> a) -> ProductCode -> a
  productCode w _ (Widget x) = w x
  productCode _ g (Gizmo x)  = g x


namespace UnitQuantity
  ||| Unit Quantity is between 1 and 1000
  public export
  data UnitQuantity : Type where
    MkUnitQuantity : Nat -> UnitQuantity

  public export
  create : Nat -> Result UnitQuantity String
  create q = if q < 1    then (Error "Unit Quantity can not be zero")
        else if q > 1000 then (Error "Unit Quantity can not be more than 1000")
        else                  Ok (MkUnitQuantity q)

  public export
  data UnitQuantityI : Nat -> Type where
    MkUnitQuantityI : { auto ok : (0 < n && n < 1000 = True) } -> UnitQuantityI n

  public export
  convertI : {n : Nat} -> UnitQuantityI n -> UnitQuantity
  convertI {n} MkUnitQuantityI = MkUnitQuantity n

  public export
  value : UnitQuantity -> Nat
  value (MkUnitQuantity k) = k


namespace KilogramQuantity
  ||| Kilogram Quantity is between 0.05 and 100.00
  export
  data KilogramQuantity : Type where
    MkKilogramQuantity : Double -> KilogramQuantity

  public export
  create : Double -> Result KilogramQuantity String
  create q = if q <   0.05 then Error "Kilogram quantity can not be less than 0.05"
        else if q > 100.00 then Error "Kilogram quantity can not be greater than 100.00"
        else Ok (MkKilogramQuantity q)

  public export
  data KilogramQuantityI : Double -> Type where
    MkKilogramQuantityI
      : { auto ok : (0.05 < q && q < 100.00) = True }
      -> KilogramQuantityI q

  public export
  convertI : {q : Double} -> KilogramQuantityI q -> KilogramQuantity
  convertI {q} MkKilogramQuantityI = MkKilogramQuantity q

  public export
  value : KilogramQuantity -> Double
  value (MkKilogramQuantity k) = k


namespace OrderQuantity
  public export
  data OrderQuantity : Type where
    Unit     : UnitQuantity     -> OrderQuantity
    Kilogram : KilogramQuantity -> OrderQuantity

  public export
  value : OrderQuantity -> Double
  value (Unit u)     = cast $ UnitQuantity.value u
  value (Kilogram k) = KilogramQuantity.value k


data CustomerId : Type where
  MkCustomerId : Int -> CustomerId

namespace EmailAddress
  export
  data EmailAddress : Type where
    MkEmailAddress : String -> EmailAddress

  export
  create : String -> Result EmailAddress String
  create s =
    if isInfixOf "@" s
      then Ok $ MkEmailAddress s
      else Error "Email address did not contain @."

  export
  value : EmailAddress -> String
  value (MkEmailAddress s) = s

namespace OrderId
  export
  data OrderId : Type where
    MkOrderId : String -> OrderId

  public export
  data OrderIdError : Type where
    MkOrderIdError : String -> OrderIdError

  export
  create : String -> Result OrderId OrderIdError
  create s =
         if s == ""       then Error $ MkOrderIdError "OrderId must not be null or empty"
    else if length s > 50 then Error $ MkOrderIdError "OrderId must not be more than 50 chars"
    else Ok $ MkOrderId s

  value : OrderId -> String
  value (MkOrderId str) = str

data ProductId : Type where
  MkProductId : Int -> ProductId

namespace OrderLineId
  export
  data OrderLineId : Type where
    MkOrderLineId : String -> OrderLineId

  public export
  data OrderLineIdError : Type where
    MkOrderLineIdError : String -> OrderLineIdError

  export
  create : String -> Result OrderLineId OrderLineIdError
  create s = Ok $ MkOrderLineId s -- TODO

  public export
  value : OrderLineId -> String
  value (MkOrderLineId s) = s


public export
data UnvalidatedAddress : Type where
  MkUnvalidatedAddress : String -> UnvalidatedAddress

namespace ZipCode
  export
  data ZipCode : Type where
    MkZipCode : String -> ZipCode

  public export
  data ZipCodeError : Type where
    MkZipCodeError : String -> ZipCodeError

  export
  create : String -> Result ZipCode ZipCodeError
  create s = if s == ""
    then Error $ MkZipCodeError "Empty ZipCode"
    else Ok $ MkZipCode s

  export
  value : ZipCode -> String
  value (MkZipCode s) = s

public export
record Address where
  constructor MkAddress
  AddressLine1 : String
  AddressLine2 : Maybe String
  AddressLine3 : Maybe String
  AddressLine4 : Maybe String
  City         : String
  ZipCode      : ZipCode

public export
data ShippingAddress : Type where
  MkShippingAddress : Address -> ShippingAddress

namespace Price
  
  export
  record Price where
    constructor MkPrice
    Value : Double

  public export
  data PriceError : Type where
    MkPriceError : String -> PriceError

  -- TODO: Add validation
  public export
  create : Double -> Result Price PriceError
  create d = if (d >= 0.0)
    then Ok $ MkPrice d
    else Error $ MkPriceError "Negative price"

  public export
  multiply : Double -> Price -> Result Price PriceError
  multiply d (MkPrice x) = create (d * x)

  public export
  value : Price -> Double
  value (MkPrice d) = d


namespace BillingAmount

  public export
  record BillingAmount where
    constructor MkBillingAmount
    Amount : Double

  public export
  data BillingAmountError : Type where
    MkBillingAmountError : String -> BillingAmountError

  public export
  create : Double -> Result BillingAmount BillingAmountError
  create d = if (d >= 0.0)
    then Ok $ MkBillingAmount d
    else Error $ MkBillingAmountError "Negative amount"

  public export
  sumPrices : List Price -> Result BillingAmount BillingAmountError
  sumPrices = create . sum . map Price.value

  public export
  value : BillingAmount -> Double
  value (MkBillingAmount v) = v


public export
data BillingAddress : Type where -- TODO
  MkBillingAddress : BillingAddress

public export
record UnvalidatedOrderLine where
  constructor MkUnvalidatedOrderLine
  OrderLineId   : String
  OrderId       : OrderId
  ProductCode   : String
  Quantity      : Double
  Price         : Price

record OrderLine where
  constructor MkOrderLine
  OrderLineId   : OrderLineId
  OrderId       : OrderId
  ProductCode   : ProductCode
  OrderQuantity : OrderQuantity
  Price         : Price

public export
record UnvalidatedCustomerInfo where
  constructor MkUnvalidatedCustomerInfo
  FirstName     : String
  LastName      : String
  EmailAddress  : String

namespace UnvalidatedOrder
  public export
  record UnvalidatedOrder where
    constructor MkUnvalidatedOrder
    OrderId         : String
    CustomerInfo    : UnvalidatedCustomerInfo
    ShippingAddress : UnvalidatedAddress
    BillingAddress  : UnvalidatedAddress
    -- Lines           : Subset (List UnvalidatedOrderLine) NonEmpty
    Lines           : List UnvalidatedOrderLine

public export
record ValidatedOrderLine where
  constructor MkValidatedOrderLine
  OrderLineId : OrderLineId
  ProductCode : ProductCode
  Quantity    : OrderQuantity

public export
record PersonalName where
  constructor MkPersonalName
  FirstName : String
  LastName  : String

public export
record CustomerInfo where
  constructor MkCustomerInfo
  Name         : PersonalName
  EmailAddress : EmailAddress

namespace ValdiatedOrder
  public export
  record ValidatedOrder where
    constructor MkValidatedOrder
    Id              : OrderId
    CustomerInfo    : CustomerInfo
    ShippingAddress : ShippingAddress
    BillingAddress  : BillingAddress
    -- OrderLines      : Subset (List ValidatedOrderLine) NonEmpty -- !!! Wow !!! TODO: Bring back this
    OrderLines      : List ValidatedOrderLine

public export
record PricedOrderLine where
  constructor MkPricedOrderLine
  OrderLineId : OrderLineId
  ProductCode : ProductCode
  Quantity    : OrderQuantity
  LinePrice   : Price

namespace PricedOrder
  public export
  record PricedOrder where
    constructor MkPricedOrder
    OrderId         : OrderId
    CustomerInfo    : CustomerInfo
    ShippingAddress : ShippingAddress
    BillingAddress  : BillingAddress
    -- OrderLines      : Subset (List PricedOrderLine) NonEmpty -- !!! Wow !!! TODO: Bring back this.
    OrderLines      : List PricedOrderLine   
    AmountToBill    : BillingAmount

data Order : Type where
  Unvalidated : UnvalidatedOrder  -> Order
  Valdiated   : ValidatedOrder    -> Order
  Proced      : PricedOrder       -> Order

OrderPlaced : Type
OrderPlaced = PricedOrder

public export
record ValidationError where
  constructor MkValidationError
  FieldName        : String
  ErrorDescription : String

namespace PlaceOrderError
  export
  data PlaceOrderError : Type where
    ValidationError : List ValidationError -> PlaceOrderError

data QuoteForm : Type where -- TODO
data OrderForm : Type where -- TODO
data ProductCatalog : Type where -- TODO

namespace CategorizedMail
  data CategorizedMail : Type where
    Quote : QuoteForm -> CategorizedMail
    Order : OrderForm -> CategorizedMail

record CalculatePricesInput where
  constructor MkCalculatePricesInput
  OrderForm      : OrderForm
  ProductCatalog : ProductCatalog

ValidationResponse : Type -> Type
ValidationResponse a =
  { e : Environment }
  -> Has [Validation] e
  -> App e (Result a (List ValidationError))

ValidateOrder : Type
ValidateOrder = UnvalidatedOrder -> ValidationResponse ValidatedOrder

data UnpaidInvoice : Type where -- TODO
data PaidInvoice : Type where -- TODO

data InvoiceInfo : Type where
  Unpaid : UnpaidInvoice -> InvoiceInfo
  Paid   : PaidInvoice   -> InvoiceInfo

data InvoiceId : Type where -- TODO

record Invoice where
  constructor MkInvoice
  InvoiceId   : InvoiceId
  InvoiceInfo : InvoiceInfo

data PhoneNumber : Type where -- TODO

data ContactId : Type where
  MkContactId : Int -> ContactId

Eq ContactId where
  (MkContactId i1) == (MkContactId i2) = i1 == i2

record Contact where
  constructor MkContact
  ContactId    : ContactId
  PhoneNumber  : PhoneNumber
  EmailAddress : EmailAddress

Eq Contact where
  (MkContact cid1 _ _) == (MkContact cid2 _ _) = cid1 == cid2

--------------------------------------------------------------------------------

data VerifiedEmailAddress : Type where -- TODO

data CustomerEmailAddress : Type where
  Unverified : EmailAddress -> CustomerEmailAddress
  Verified : VerifiedEmailAddress -> CustomerEmailAddress

data EmailContactInfo : Type where -- TODO
data PostalContactInfo : Type where -- TODO

record BothContactMethods where
  constructor MkBothContactMethods
  Email : EmailContactInfo
  Address : PostalContactInfo

data ContactInfo : Type where
  EmailOnly    : EmailContactInfo   -> ContactInfo
  AddrOnly     : PostalContactInfo  -> ContactInfo
  EmailAndAddr : BothContactMethods -> ContactInfo

data Name : Type where -- TODO

record Contact1 where
  constructor MkContact1
  Name : Name
  ContactInfo : ContactInfo

--------------------------------------------------------------------------------
