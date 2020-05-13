module Network.Ethereum.Contract.Groveler where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Language.Solidity.Abi          ( Declaration(..)
                                                , ContractAbi(..)
                                                , EventArg(..)
                                                , FunctionArg(..)
                                                )
import Network.Ethereum.Api.Types (Change(..))

import           Network.Ethereum.Contract.Groveler.Types

grovelCommandToIO :: ContractAbi -> GrovelCommand -> Either Text [Change -> IO (Either Text ())]
grovelCommandToIO cabi = \case
  PeriodicStateGroveler _ ->
    Left "PeriodicStateUpdate's are currently unsupported."
  EventGroveler evg -> do
    evdec <- lookUpEventDec cabi (eventGrovelName evg)
    mapM (actionToIO cabi evdec) (eventGrovelActions evg)

lookUpEventDec :: ContractAbi -> EventName -> Either Text EventDec
lookUpEventDec = undefined

actionToIO :: ContractAbi -> EventDec -> Action -> Either Text (Change -> IO (Either Text ()))
actionToIO cabi evdec = \case
  ActionFunctionCall fc -> do
    fdec <- lookUpFunctionDec cabi (functionCallName fc)
    mkFunctionCallIO fdec evdec fc
  _ -> pure (const $ pure (Left "actionToIO: Only ActionFunctionCalls are supported"))

mkFunctionCallIO :: FunctionDec -> EventDec -> FunctionCall -> Either Text (Change -> IO (Either Text ()))
mkFunctionCallIO fdec evdec FunctionCall {..} = do
  pairedArgs <- mapM (pairFunctionAndEventArgs fdec evdec) functionCallArgs
  pure $ changeToIO pairedArgs
 where
  changeToIO pairArgs change = case parseChange evdec change of
    Right eventArgSolValues -> pure $ Right ()
    Left err -> pure $ Left err

pairFunctionAndEventArgs :: FunctionDec -> EventDec -> FunctionArgument -> Either Text [(FunctionArg, EventArg)]
pairFunctionAndEventArgs = undefined

parseChange :: EventDec -> Change -> Either Text [SolidityValue]
-- See: https://github.com/charlescrain/hs-web3/blob/raw-event-listener/src/Data/Solidity/Event.hs
parseChange evdec ch = undefined

lookUpFunctionDec :: ContractAbi -> FunctionCallName -> Either Text FunctionDec
lookUpFunctionDec = undefined

