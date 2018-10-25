-- | GHCJS client for talking to a Pact backend for verification.
module Pact.Analyze.WebSocket.Client where

testing :: IO String
testing = pure "test"

verifyModule
  :: HM.HashMap ModuleName ModuleData   -- ^ all loaded modules
  -> ModuleData                         -- ^ the module we're verifying
  -> IO [Text]

