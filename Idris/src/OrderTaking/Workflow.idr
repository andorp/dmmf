module OrderTaking.Workflow -- 'where' is not needed

import Control.App
import Data.Bifunctor
import Data.Strings

import Common.App
import Common.Prelude
import OrderTaking.Domain

--------------------------------------------------------------------------------
--
-- workflow "Place Order" =
--   input: UnvalidatedOrder
--   output (on success):
--     OrderAcknoledgementSent
--     AND OrderPlaced (to send to shipping)
--     AND BillableOrderPlaced (to send to billing)
--   output (on error):
--     ValidationError
--
--   step 1
--   do ValidateOrder
--   if order is invalied then:
--     return with ValidationError
--
--   step 2
--   do PriceOrder
--
--   step 3
--   do AcknowledgeOrder
--
--   step 4
--   create and return the events
--
--------------------------------------------------------------------------------

public export
PlaceOrderCommand : Type
PlaceOrderCommand = Command UnvalidatedOrder

-- ChangeOrder : Type
-- ChangeOrder = Command Void

-- CancelOrder : Type
-- CancelOrder = Command Void

-- data OrderTakingCommand : Type where
--   Place  : PlaceOrder  -> OrderTakingCommand
--   Change : ChangeOrder -> OrderTakingCommand
--   Cancel : CancelOrder -> OrderTakingCommand

--------------------------------------------------------------------------------
-- substep "ValidateOrder" =
--   input: UnvalidatedOrder
--   output: ValidatedOrder OR ValidationError
--   dependencies: CheckProductCodeExists, CheckAddressExists
--------------------------------------------------------------------------------

CheckProductCodeExists : Type
CheckProductCodeExists =
  { e : Environment }
  -> ProductCode
  -> App e Bool

namespace AddressInfo
  record AddressInfo where
    constructor MkAddressInfo
    AddressLine1 : String
    AddressLine2 : String
    AddressLine3 : String
    AddressLine4 : String
    City         : String
    ZipCode      : String

data CheckedAddress : Type where
  MkCheckedAddress : AddressInfo -> CheckedAddress

data AddressValidationError : Type where
  MkAddressValidationError : String -> AddressValidationError

CheckAddressExists : Type
CheckAddressExists =
  { e : Environment }
  -> UnvalidatedAddress
  -> App e (Result CheckedAddress AddressValidationError)

ValidateOrder : Type
ValidateOrder =
  { e : Environment }    ->
  CheckProductCodeExists -> -- Dependency
  CheckAddressExists     -> -- Dependency
  UnvalidatedOrder       -> -- Input
  App e (Result ValidatedOrder ValidationError)

--------------------------------------------------------------------------------
-- substep "PriceOrder" =
--   input: ValidatedOrder
--   output: PricedOrder
--   dependencies: GetProductPrice
--------------------------------------------------------------------------------

GetProductPrice : Type
GetProductPrice =
  { e : Environment }
  -> ProductCode
  -> App e Price

data PricingError : Type where
  MkPricingError : String -> PricingError

PriceOrder : Type
PriceOrder =
  { e : Environment } ->
  GetProductPrice     -> -- Dependency
  ValidatedOrder      -> -- Input
  App e (Result PricedOrder PricingError)

--------------------------------------------------------------------------------
-- substep "AcknowledgeOrder" =
--   input: PricedOrder
--   output: OrderAcknowledgementSent
--   dependencies: SendOrderAcknoledgement, CreateOrderAcknowledgementLetter
--------------------------------------------------------------------------------

data HtmlString : Type where
  MkHtmlString : String -> HtmlString

record OrderAcknowledgement where
  constructor MkOrderAcknowledgement
  EmailAddress : EmailAddress
  -- ISSUE: when email address is private, its name is still leaking out
  -- and ambiguity checker complains about it:
  -- Ambiguous name [OrderTaking.Domain.Contact.EmailAddress, OrderTaking.Domain.EmailAddress] at:
  -- 137       EmailAddress : EmailAddress
  Letter : HtmlString

-- TODO: Which one is the better? ...Dep or ...I ?
CreateOrderAcknowledgementLetter : Type
CreateOrderAcknowledgementLetter =
  { e : Environment }
  -> PricedOrder
  -> App e HtmlString

data SendResult : Type where
  Sent    : SendResult
  NotSent : SendResult

SendOrderAcknowledgement : Type
SendOrderAcknowledgement =
  { e : Environment }
  -> OrderAcknowledgement
  -> App e SendResult

record OrderAcknowledgementSent where
  constructor MkOrderAcknowledgementSent
  OrderId : OrderId
  EmailAddress : EmailAddress

AcknowledgeOrder : Type
AcknowledgeOrder =
  { e : Environment } ->
  CreateOrderAcknowledgementLetter -> -- Dependency
  SendOrderAcknowledgement         -> -- Dependency
  PricedOrder                      -> -- Input
  App e (Maybe OrderAcknowledgementSent)

--------------------------------------------------------------------------------
-- Creating return events
--------------------------------------------------------------------------------

OrderPlaced : Type
OrderPlaced = PricedOrder

record BillableOrderPlaced where
  constructor MkBillableOrderPlaced
  OrderId         : OrderId
  BillingAddress  : BillingAddress
  AmountToBill    : BillingAmount

data PlaceOrderEvent : Type where
  MkOrderPlacedEvent         : OrderPlaced                     -> PlaceOrderEvent
  MkBillableOrderPlacedEvent : BillableOrderPlaced             -> PlaceOrderEvent
  MkAcknowledgementSentEvent : Maybe OrderAcknowledgementSent  -> PlaceOrderEvent

CreateEvents : Type
CreateEvents =
  { e : Environment }
  -> PricedOrder
  -> Maybe OrderAcknowledgementSent
  -> App e (List PlaceOrderEvent)

--------------------------------------------------------------------------------
-- Effects in Validation step.
--
-- TODO: How to handle async requests.
-- I think this will be part of the App implementation and we don't have to
-- count that in.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- QUESTION: Dependencies as implicit parameters?
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PUBLIC API
--------------------------------------------------------------------------------

-- * Functions exposed in a public API, hide dependency information from callers
-- * For functions used internally, be explicit about their dependencies

public export
PlaceOrderWorkflow : Type
PlaceOrderWorkflow =
  { e : Environment }
  -> PlaceOrderCommand
  -> App e (Result (List PlaceOrderEvent) PlaceOrderError)

--------------------------------------------------------------------------------
-- Implementation
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- validateOrder
--------------------------------------------------------------------------------

-- Throws pattern match exception on non valid data.
toCustomerInfo
  : { e : Environment }
  -> UnvalidatedCustomerInfo
  -> App e (Result CustomerInfo ValidationError)
toCustomerInfo customer = do
  Ok firstName <- pure $ String50.create $ FirstName customer
  | Error (MkString50Error err) => pure $ Error $ MkValidationError "First name" err

  Ok lastName <- pure $ String50.create $ LastName customer
  | Error (MkString50Error err) => pure $ Error $ MkValidationError "Last name" err

  Ok emailAddress <- pure $ EmailAddress.create $ EmailAddress customer
  | Error err => pure $ Error $ MkValidationError "Email" err

  let name = MkPersonalName firstName lastName
  let customerInfo = MkCustomerInfo name emailAddress

  pure $ Ok customerInfo

--------------------------------------------------------------------------------

data AddressError : Type where
  MkAddressError : String -> AddressError

toAddress
  :  { e : Environment }
  -> CheckAddressExists
  -> UnvalidatedAddress
  -> App e (Result Address AddressError)
toAddress checkAddressExists unvalidatedAddress = do
  Ok (MkCheckedAddress checkedAddress) <- checkAddressExists unvalidatedAddress
  | Error (MkAddressValidationError err) => pure $ Error $ MkAddressError err

  Ok addressLine1 <- pure $ String50.create $ AddressLine1 checkedAddress
  | Error (MkString50Error err) => pure $ Error $ MkAddressError err

  Ok addressLine2 <- pure $ String50.createMaybe $ AddressLine2 checkedAddress
  | Error (MkString50Error err) => pure $ Error $ MkAddressError err

  Ok addressLine3 <- pure $ String50.createMaybe $ AddressLine3 checkedAddress
  | Error (MkString50Error err) => pure $ Error $ MkAddressError err

  Ok addressLine4 <- pure $ String50.createMaybe $ AddressLine4 checkedAddress
  | Error (MkString50Error err) => pure $ Error $ MkAddressError err

  Ok city <- pure $ String50.create $ City checkedAddress
  | Error (MkString50Error err) => pure $ Error $ MkAddressError err

  Ok zipCode <- pure $ ZipCode.create $ ZipCode checkedAddress
  | Error (MkZipCodeError err) => pure $ Error $ MkAddressError err

  pure $ Ok $ MkAddress
    addressLine1
    addressLine2
    addressLine3
    addressLine4
    city
    zipCode

--------------------------------------------------------------------------------

data ValidatedOrderLineError : Type where
  MkValidatedOrderLineError : String -> ValidatedOrderLineError

toProductCode
  : { e : Environment }
  -> CheckProductCodeExists
  -> String
  -> App e (Result ProductCode String)
toProductCode checkProductCodeExists productCode = do
  Ok code <- pure $ ProductCode.create productCode
  | Error (MkProductCodeError err) => pure $ Error err

  True <- checkProductCodeExists code
  | False => pure $ Error "Invalid Product Code"

  pure $ Ok code

toOrderQuantity
  :  ProductCode
  -> Double
  -> Result OrderQuantity String
toOrderQuantity (Widget _) quantity
  = map @{ResultSuccessFunctor} Unit $ UnitQuantity.create $ fromInteger $ cast $ quantity
toOrderQuantity (Gizmo _) quantity
  = map @{ResultSuccessFunctor} Kilogram $ KilogramQuantity.create $ quantity

toValidateOrderLine
  :  { e : Environment }
  -> CheckProductCodeExists
  -> UnvalidatedOrderLine
  -> App e (Result ValidatedOrderLine String)
toValidateOrderLine checkProductCodeExists unvalidatedOrderLine = do

  Ok orderLineId <- pure $ OrderLineId.create $ OrderLineId unvalidatedOrderLine
  | Error (MkOrderLineIdError err) => pure $ Error err

  Ok productCode <- toProductCode checkProductCodeExists $ ProductCode unvalidatedOrderLine
  | Error err => pure $ Error err

  Ok quantity <- pure $ toOrderQuantity productCode $ Quantity unvalidatedOrderLine
  | Error err => pure $ Error err

  pure $ Ok $ MkValidatedOrderLine
    orderLineId
    productCode
    quantity

--------------------------------------------------------------------------------

validateOrder : ValidateOrder
validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder = do
  Ok orderId <- pure $ OrderId.create $ OrderId unvalidatedOrder
  | Error (MkOrderIdError m) => pure $ Error $ MkValidationError "OrderId" m

  Ok customerInfo <- toCustomerInfo $ CustomerInfo unvalidatedOrder
  | Error err => pure $ Error err

  Ok shippingAddress <- toAddress checkAddressExists $ ShippingAddress unvalidatedOrder
  | Error (MkAddressError err) => pure $ Error $ MkValidationError "Shipping Address" err

  (orderLines, []) -- : (List (Result ValidatedOrderLine String))
    <- map partitionResults
     $ traverse (toValidateOrderLine checkProductCodeExists)
     $ Lines unvalidatedOrder
  | (_, errors) => pure $ Error $ MkValidationError "Order Lines" $ unlines errors

  let billingAddress  = MkBillingAddress

  pure $ Ok $ MkValidatedOrder
    orderId
    customerInfo
    (MkShippingAddress shippingAddress)
    billingAddress
    orderLines

--------------------------------------------------------------------------------
-- priceOrder
--------------------------------------------------------------------------------

toPricedOrderLine
  :  { e : Environment }
  -> GetProductPrice
  -> ValidatedOrderLine
  -> App e (Result PricedOrderLine String)
toPricedOrderLine getProductPrice line = do
  let qty = OrderQuantity.value $ Quantity line
  price <- getProductPrice $ ProductCode line
  Ok linePrice <- pure $ Price.multiply qty price
  | Error (MkPriceError err) => pure $ Error err
  pure $ Ok $ MkPricedOrderLine
    (OrderLineId line)
    (ProductCode line)
    (Quantity    line)
    linePrice

priceOrder : PriceOrder
priceOrder getProductPrice validatedOrder = do

  (lines, []) <- map partitionResults
               $ traverse (toPricedOrderLine getProductPrice)
               $ OrderLines validatedOrder
  | (_, errors) => pure $ Error $ MkPricingError $ unlines errors

  Ok amountToBill <- pure $ BillingAmount.sumPrices $ map (\line => LinePrice line) lines
  | Error (MkBillingAmountError err) => pure $ Error $ MkPricingError err

  pure $ Ok $ MkPricedOrder
    (Id              validatedOrder)
    (CustomerInfo    validatedOrder)
    (ShippingAddress validatedOrder)
    (BillingAddress  validatedOrder)
    lines
    amountToBill

--------------------------------------------------------------------------------
-- acknowledgeOrder
--------------------------------------------------------------------------------

acknowledgeOrder : AcknowledgeOrder
acknowledgeOrder createAcknowledgementLetter sendAcknowledgement pricedOrder = do
  letter <- createAcknowledgementLetter pricedOrder
  let acknowledgement
      = MkOrderAcknowledgement
        (EmailAddress $ CustomerInfo pricedOrder)
        letter
  sent <- sendAcknowledgement acknowledgement
  pure $ case sent of
    NotSent => Nothing
    Sent => Just $ MkOrderAcknowledgementSent
              (OrderId pricedOrder)
              (EmailAddress $ CustomerInfo pricedOrder)

--------------------------------------------------------------------------------
-- Creating events
--------------------------------------------------------------------------------

createBillingEvent : PricedOrder -> Maybe BillableOrderPlaced
createBillingEvent placedOrder =
  let billingAmount = BillingAmount.value $ AmountToBill placedOrder
  in if billingAmount > 0.0
        then Just $ MkBillableOrderPlaced
                      (OrderId        placedOrder)
                      (BillingAddress placedOrder)
                      (AmountToBill   placedOrder)
        else Nothing


--------------------------------------------------------------------------------
-- placeOrder
--------------------------------------------------------------------------------

{-
placeOrder : PlaceOrderWorkflow
placeOrder unvalidatedOrder = do
  validatedOrder        <- validateOrder    checkProductCodeExists checkAddressExists
  pricedOrder           <- priceOrder       getProductPrice
  acknowledgementOption <- acknowledgeOrder createAcknowledgementLetter sendAcknowledgement
  createEvents pricedOrder acknowledgementOption
-}

-- Page 175

