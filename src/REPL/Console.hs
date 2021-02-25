{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module REPL.Console where

import           Import
import           RIO.Orphans                    ( )
import           Data.Text                     as Text
import           System.Console.Haskeline       ( InputT
                                                , Settings
                                                , getInputLine
                                                , runInputT
                                                )
import           System.Console.Pretty          ( Color(Green, Red)
                                                , Pretty(color, style)
                                                , Style(Faint)
                                                )
import           Lang.Parser
import           REPL.Eval

startREPL :: Settings (RIO App) -> RIO App ()
startREPL settings = runInputT settings $ runLine Green

runLine :: Color -> InputT (RIO App) ()
runLine colour = do
  line <- getInputLine $ (style Faint . color colour) "S48" <> "> "
  checkLine $ strip . fromString <$> line

checkLine :: Maybe Text -> InputT (RIO App) ()
checkLine (Just "quit") = return ()
checkLine (Just line)
  | Text.null line = runLine Green
  | otherwise = evalString line >>= \case
    Left  err -> lift (logError (fromString $ show err)) >> runLine Red
    Right val -> lift (logInfo (fromString val)) >> runLine Green
checkLine _ = return ()

evalString :: Text -> InputT (RIO a) (ThrowsError String)
evalString expr = return $ readExpr expr >>= eval <&> show
