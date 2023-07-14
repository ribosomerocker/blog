module Main where

import Control.Lens
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Lens as A
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Forward
import Development.Shake.FilePath
import GHC.Generics
import Slick
import qualified Data.Text as T
import Data.Aeson.KeyMap (union)


data SiteMeta = SiteMeta { baseUrl :: String
                         , siteTitle :: String
                         , githubUser :: String
                         , mastodonHandle :: String
                         }
                deriving (Generic, Eq, Ord, Show, ToJSON)


data IndexInfo = IndexInfo { posts :: [Post] } 
  deriving (Generic, Show, FromJSON, ToJSON)

data Post = Post { title :: String
                 , content :: String
                 , url :: String
                 , date :: String
                 , image :: Maybe String
                 }
            deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

siteMeta :: SiteMeta
siteMeta = SiteMeta { baseUrl = "denotation.pages.dev"
                    , siteTitle = "Denotation"
                    , githubUser = "ribosomerocker"
                    , mastodonHandle = "@verdigris"
                    }

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

main :: IO ()
main = do
  let shOpts = 
        forwardOptions $ shakeOptions
          { shakeVerbosity = Verbose
          , shakeLintInside = ["./web/"]
          }
  shakeArgsForward shOpts buildRules


outputFolder :: FilePath
outputFolder = "output/"

buildIndex :: [Post] -> Action ()
buildIndex posts = do
  indexT <- compileTemplate' "web/templates/index.html"
  let indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON (IndexInfo { posts }))
  writeFile' (outputFolder </> "index.html") indexHTML

buildPosts :: Action [Post]
buildPosts = do
  paths <- getDirectoryFiles "." ["web/posts//*.md"]
  forP paths buildPost

buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl

  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta (withPostUrl postData)
  template <- compileTemplate' "web/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  -- Convert the metadat into a Post object
  convert fullPostData


copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./web/" ["images//*", "css//*"]
  void $ forP filepaths \paths ->
    copyFileChanged ("web" </> paths) (outputFolder </> paths)

buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildIndex allPosts
  copyStaticFiles

