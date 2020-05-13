module Network.Ethereum.Contract.Groveler.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Solidity.Abi (Declaration(..), ContractAbi(..),EventArg(..), FunctionArg(..))
import Data.Solidity.Prim.Int (IntN, UIntN)
import Data.Solidity.Prim.Bytes (Bytes, BytesN)
import Data.Solidity.Prim.Address (Address)


data Abi = Abi { abi :: ContractAbi
               , abiName :: AbiName 
               }
newtype AbiText = AbiText {unAbiText :: Text}
newtype AbiName = AbiName {unAbiName :: Text}

data AddAbiBody = AddAbiBody
  { addAbiBodyName :: AbiName
  , addAbiBodyAbi  :: AbiText
  }

data BlockNumber = BlockNumber
data HexString = HexString
data Hash = Hash

newtype ContractAddress = ContractAddress {unContractAddress :: HexString}
data IndexBody = IndexBody
  { indexBodyContractAddress :: ContractAddress
  , indexBodyStartBlock :: Maybe BlockNumber
  , indexBodyStartTransactionHash :: Maybe Hash
  , indexBodyAbiName :: AbiName
  , indexBodyGrovelArgs :: [ GrovelCommand ]
  }

data GrovelCommand = EventGroveler EventGrovel
                   | PeriodicStateGroveler PeriodicStateGrovel

data PeriodicStateGrovel = PeriodicStateGrovel

newtype EventName = EventName {unEventName :: Text}
newtype EventArgumentName = EventArgumentName {unEventArgumentName :: Text}

data EventGrovel = EventGrovel
  { eventGrovelName :: EventName
  , eventGrovelActions :: [ Action ]
  }

newtype FunctionCallName = FunctionCallName {unFunctionCallName :: Text}

-- data EventAction = EventAction [EventArgumentName] Action

data Action = ActionFunctionCall FunctionCall
            | ActionContractCall ContractAction 


data ContractAddressSource = LiteralAddress ContractAddress
                           | FromEventArgument EventArgumentName

data ContractAction = ContractAction
  { contractActionContractAddressSource :: ContractAddressSource
  , contractActionAbiName :: AbiName
  , contractActionFunctionCallName :: FunctionCall
  }

data FunctionCall = FunctionCall
  { functionCallName :: FunctionCallName
  , functionCallArgs :: [FunctionArgument]
  }

newtype FunctionArgumentName = FunctionArgumentName {unFunctionArgumentName :: Text}
data FunctionArgument = FunctionArgument FunctionArgumentName FunctionArgumentSource

newtype FunctionArgumentSource = EventArgumentSource EventArgumentName


data FunctionDec = FunctionDec
  { functionDecName :: Text
  , functionDecConstant :: Bool
  , functionDecInputs :: [FunctionArg]
  , functionDecOutputs :: Maybe [FunctionArg]
  }

decToFunctionDec :: Declaration -> Either Text FunctionDec
decToFunctionDec DFunction {..} =
  pure $ FunctionDec funName funConstant funInputs funOutputs
decToFunctionDec dec =
  Left $ "Only DFunction Declarations can be a FunctionDec. Found: " <> T.pack
    (show dec)

data EventDec = EventDec
  { eventDecName :: Text
  , eventDecInputs :: [EventArg]
  , eventDecAnonymous :: Bool
  }

decToEventDec :: Declaration -> Either Text EventDec
decToEventDec DEvent {..} =
  pure $ EventDec eveName eveInputs eveAnonymous
decToEventDec dec =
  Left $ "Only DEvent Declarations can be an EventDec. Found: " <> T.pack
    (show dec)


data EventArgumentValue = EventArgumentValue
  { eventArgumentValueArgumentName :: EventArgumentName
  , eventArgumentValueArgumentValue :: SolidityValue
  }


data SolidityIntValue a = SolidityInt8 (a 8)
                        | SolidityInt16 (a 16)
                        | SolidityInt24 (a 24)
                        | SolidityInt32 (a 32)
                        | SolidityInt40 (a 40)
                        | SolidityInt48 (a 48)
                        | SolidityInt56 (a 56)
                        | SolidityInt64 (a 64)
                        | SolidityInt72 (a 72)
                        | SolidityInt80 (a 80)
                        | SolidityInt88 (a 88)
                        | SolidityInt96 (a 96)
                        | SolidityInt104 (a 104)
                        | SolidityInt112 (a 112)
                        | SolidityInt120 (a 120)
                        | SolidityInt128 (a 128)
                        | SolidityInt136 (a 136)
                        | SolidityInt144 (a 144)
                        | SolidityInt152 (a 152)
                        | SolidityInt160 (a 160)
                        | SolidityInt168 (a 168)
                        | SolidityInt176 (a 176)
                        | SolidityInt184 (a 184)
                        | SolidityInt192 (a 192)
                        | SolidityInt200 (a 200)
                        | SolidityInt208 (a 208)
                        | SolidityInt216 (a 216)
                        | SolidityInt224 (a 224)
                        | SolidityInt232 (a 232)
                        | SolidityInt240 (a 240)
                        | SolidityInt248 (a 248)
                        | SolidityInt256 (a 256)

data SoliditySizedBytesValue =  SolidityBytes1 (BytesN 1)
                             | SolidityBytes2 (BytesN 2)
                             | SolidityBytes3 (BytesN 3)
                             | SolidityBytes4 (BytesN 4)
                             | SolidityBytes5 (BytesN 5)
                             | SolidityBytes6 (BytesN 6)
                             | SolidityBytes7 (BytesN 7)
                             | SolidityBytes8 (BytesN 8)
                             | SolidityBytes9 (BytesN 9)
                             | SolidityBytes10 (BytesN 10)
                             | SolidityBytes11 (BytesN 11)
                             | SolidityBytes12 (BytesN 12)
                             | SolidityBytes13 (BytesN 13)
                             | SolidityBytes14 (BytesN 14)
                             | SolidityBytes15 (BytesN 15)
                             | SolidityBytes16 (BytesN 16)
                             | SolidityBytes17 (BytesN 17)
                             | SolidityBytes18 (BytesN 18)
                             | SolidityBytes19 (BytesN 19)
                             | SolidityBytes20 (BytesN 20)
                             | SolidityBytes21 (BytesN 21)
                             | SolidityBytes22 (BytesN 22)
                             | SolidityBytes23 (BytesN 23)
                             | SolidityBytes24 (BytesN 24)
                             | SolidityBytes25 (BytesN 25)
                             | SolidityBytes26 (BytesN 26)
                             | SolidityBytes27 (BytesN 27)
                             | SolidityBytes28 (BytesN 28)
                             | SolidityBytes29 (BytesN 29)
                             | SolidityBytes30 (BytesN 30)
                             | SolidityBytes31 (BytesN 31)
                             | SolidityBytes32 (BytesN 32)
  deriving (Eq, Show)



data SolidityValue = SInt  (SolidityIntValue IntN)
                   | SUInt (SolidityIntValue UIntN)
                   | SBool Bool
                   | SBytes Bytes
                   | SBytesN SoliditySizedBytesValue
                   | SString Text
                   | SAddress Address
                   | SListN SolidityValue Int
                   | SList [SolidityValue]
