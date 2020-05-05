module App where

import Brick
import RIO
import System.Directory
import Data.Version (Version)
import qualified Data.Version as Version
import qualified Paths_wuzzkell as Meta
import qualified Graphics.Vty as V 
import Options.Applicative
import Http 
import UI
import Brick.BChan


runApp :: IO()
runApp = do
  _ <- execParser opts
  initialVty <- buildVty
  chan <- newBChan 10
  let st = initBrickState & set stAppEventChan (Just chan)  
  void $ customMain initialVty buildVty (st ^. stAppEventChan) app st
  where
    opts = info (parseConfPath <|> pure "" <**> version Meta.version <**> helper)
         $ fullDesc
        <> progDesc "Print a greeting for TARGET"
        <> header "hello - a test for optparse-applicative"
    buildVty = do
      v <- V.mkVty =<< V.standardIOConfig
      V.setMode (V.outputIface v) V.Mouse True
      pure v
      
version :: Version -> Parser (a -> a)
version v =
  infoOption (showVersion v)
    $ long "version"
   <> help "Show version"

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v
  ]

parseConfPath :: Parser FilePath
parseConfPath = strOption
  (  long "config"
  <> short 'c'
  <> metavar "FilePath"
  <> showDefault
  <> help "append config from path"
  )

