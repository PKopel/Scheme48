{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module REPL.Console where

import           Import
import           RIO.Orphans                    ( )
import           Data.Text                     as Text
                                                ( null
                                                , strip
                                                )
import           System.Console.Haskeline       ( InputT
                                                , Settings
                                                , getInputLine
                                                , runInputT
                                                )
import           System.Console.Pretty          ( Color(Green, Red)
                                                , Pretty(color, style)
                                                , Style(Faint)
                                                )
import           Lang.Parser                    ( readExpr )
import           Lang.Primitives                ( primitiveBindings )
import           REPL.Eval                      ( eval )

startREPL :: Settings (RIO App) -> RIO App ()
startREPL settings =
  primitiveBindings >>= \env -> runInputT settings $ runLine env Green

runLine :: Env -> Color -> InputT (RIO App) ()
runLine env colour = do
  line <- getInputLine $ (style Faint . color colour) "S48" <> "> "
  checkLine env $ strip . fromString <$> line

checkLine :: Env -> Maybe Text -> InputT (RIO App) ()
checkLine _ (Just "quit") = return ()
checkLine env (Just line)
  | Text.null line = runLine env Green
  | otherwise = evalString env line >>= \case
    Left  err -> lift (logError (fromString $ show err)) >> runLine env Red
    Right val -> lift (logInfo (fromString val)) >> runLine env Green
checkLine _ _ = return ()

evalString :: Env -> Text -> InputT (RIO App) (ThrowsError String)
evalString env expr =
  lift . runIOThrows $ liftThrows (readExpr expr) >>= eval env <&> show
