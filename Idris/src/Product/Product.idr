module Product.Product

import Control.App
import Control.App.Console
import Control.App.FileIO


data ProductCode : Type where
  MkProductCode : String -> ProductCode

data OrderQuantity : Type where
  UnitQuantity     : Int    -> OrderQuantity
  KilogramQuantity : Double -> OrderQuantity

namespace QuantityExamples
  public export
  anOrderQuantity : OrderQuantity
  anOrderQuantity = UnitQuantity 1

  public export
  aKilogramQuantity : OrderQuantity
  aKilogramQuantity = KilogramQuantity 2.5

public export
printQuantity : OrderQuantity -> String
printQuantity (UnitQuantity     uQty)  = show uQty ++ " units"
printQuantity (KilogramQuantity kgQty) = show kgQty ++ " kg"

data CheckNumber : Type where
  MkCheckNumber : Int -> CheckNumber

data CardNumber : Type where
  MkCardNumber : String -> CardNumber

data CardType : Type where
  Visa       : CardType
  MasterCard : CardType

record CreditCardInfo where
  constructor MkCreditCardInfo
  CreditCardType   : CardType
  CreditCardNumber : CardNumber

data PaymentMethod : Type where
  Cash  :                   PaymentMethod
  Check : CheckNumber    -> PaymentMethod
  Card  : CreditCardInfo -> PaymentMethod

data PaymentAmount : Type where
  MkPaymentAmount : Double -> PaymentAmount

data Currency : Type where
  EUR : Currency
  USD : Currency

record Payment where
  constructor MkPayment
  Amount : PaymentAmount
  Currency : Currency
  Method : PaymentMethod

data UnpaidInvoice : Type where -- TODO
data PaidInvoice : Type where -- TODO

ConvertPaymentCurrency : Type
ConvertPaymentCurrency = Payment -> Currency -> Payment

record PersonalName where
  constructor MkPersonalName
  FirstName     : String
  MiddleInitial : Maybe String
  LastName      : String

data PaymentError : Type where -- TODO

data UnpaidInvoice : Type where -- TODO
data PaidInvoice : Type where -- TODO

PayInvoice : Type
PayInvoice =
     {e : List Type}
  -> UnpaidInvoice
  -> Payment
  -> Has [FileIO] e
  => App e ()

data PaymentError : Type where
  CardTypeNotRecognized   : PaymentError
  PaymentRejected         : PaymentError
  PaymentProvideroffline  : PaymentError

data Customer : Type where

SaveCustomer : Type
SaveCustomer
  = { e : List Type }
  -> Customer
  => Has [FileIO] e
  -> App e ()

interface Random e where
  nextRandom : App {l} e Int

NextRandom : Type
NextRandom
  = { e : List Type }
  -> Int
  => Has [Random] e
  -> App e Int

data OrderId : Type where -- TODO
data OrderLine : Type where -- TODO

record Order where
  constructor MkOrder
  OrderId : OrderId
