{-# OPTIONS_GHC -Wno-deprecations #-}
module Main where

import Control.Lens ( (?~), At(at) )
import Control.Monad ( void )
import Data.Aeson as A
    ( FromJSON, Value(String, Object), ToJSON(toJSON) )
import Data.Aeson.Lens as A ( AsValue(_Object) )
import Development.Shake
    ( copyFileChanged,
      forP,
      readFile',
      writeFile',
      shakeOptions,
      getDirectoryFiles,
      liftIO,
      Action,
      ShakeOptions(shakeLintInside, shakeVerbosity),
      Verbosity(Verbose) )
import Development.Shake.Classes ( Binary )
import Development.Shake.Forward
    ( cacheAction, forwardOptions, shakeArgsForward )
import Development.Shake.FilePath ( (-<.>), (</>), dropDirectory1, takeFileName, dropExtension )
import GHC.Generics ( Generic )
import Slick
    ( substitute, compileTemplate', markdownToHTML, convert )
import qualified Data.Text as T
import Data.Aeson.KeyMap (union)
import Data.Time (defaultTimeLocale, parseTimeOrError, formatTime, iso8601DateFormat, getCurrentTime, UTCTime)
import Data.Foldable (traverse_)
import Data.Bifunctor (Bifunctor(first))
import System.Random.Stateful (uniformListM, globalStdGen)


data SiteMeta = SiteMeta { baseUrl :: String
                         , siteTitle :: String
                         , githubUser :: String
                         , mastodonHandle :: String
                         , assetHash :: String
                         }
                deriving (Generic, Eq, Ord, Show, ToJSON)


data BlogInfo = BlogInfo { posts :: [Post] }
  deriving (Generic, Show, FromJSON, ToJSON)

data Post = Post { title :: String
                 , content :: String
                 , url :: String
                 , date :: String
                 , description :: String
                 }
            deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data AtomData =
  AtomData { title        :: String
           , domain       :: String
           , posts        :: [Post]
           , currentTime  :: String
           , atomUrl      :: String }
    deriving (Generic, ToJSON, Eq, Ord, Show)


siteMeta :: IO SiteMeta
siteMeta = do
  (rand :: String) <- uniformListM 10 globalStdGen

  pure $ SiteMeta { baseUrl = "denotation.pages.dev"
                  , siteTitle = "Denotation"
                  , githubUser = "ribosomerocker"
                  , mastodonHandle = "@verdigris"
                  , assetHash = rand
                  }

withSiteMeta :: Value -> SiteMeta -> Value
withSiteMeta (Object obj) meta = Object $ union obj siteMetaObj
  where
    Object siteMetaObj = toJSON meta
withSiteMeta _ _ = error "only add site meta to objects"

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

buildBlog :: [Post] -> Action ()
buildBlog posts = do
  paths <- getDirectoryFiles "." [ "web/templates//*.main" ]
  meta <- liftIO siteMeta
  let pPaths = fmap dropExtension paths
      blogInfo = BlogInfo { posts }
      htmlify thing = T.unpack $ substitute thing (withSiteMeta (toJSON blogInfo) meta)
  things <- traverse compileTemplate' paths
  traverse_ (\(x,y) -> writeFile' (outputFolder </> x) (htmlify y)) (first takeFileName <$> zip pPaths things)

buildPosts :: Action [Post]
buildPosts = do
  paths <- getDirectoryFiles "." [ "web/posts//*.md" ]
  forP paths buildPost

buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  meta <- liftIO siteMeta
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta (withPostUrl postData) meta
  template <- compileTemplate' "web/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  -- Convert the metadat into a Post object
  convert fullPostData


copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./web/" ["css//*", "robots.txt", "assets//*"]
  void $ forP filepaths $ \paths ->
    copyFileChanged ("web" </> paths) (outputFolder </> paths)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:%SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

buildFeed :: [Post] -> Action ()
buildFeed posts' = do
  now <- liftIO getCurrentTime
  meta <- liftIO siteMeta
  let
      atomData =
        AtomData
          { title = meta.siteTitle
          , domain = meta.baseUrl
          , posts = mkAtomPost <$> posts'
          , currentTime = toIsoDate now
          , atomUrl = "/atom.xml"
          }
  atomTempl <- compileTemplate' "web/templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ substitute atomTempl (toJSON atomData)
    where
      mkAtomPost :: Post -> Post
      mkAtomPost p = p { date = formatDate $ p.date }

buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  liftIO (print allPosts)
  buildFeed allPosts
  buildBlog allPosts
  copyStaticFiles
