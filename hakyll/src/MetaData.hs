-- Taken from https://github.com/jwiegley/sitebuilder/blob/master/Main.hs
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

module MetaData where

import qualified Data.Aeson.Key as AT
import qualified Data.Aeson.KeyMap as AT
import qualified Data.Aeson.Types as AT
import Data.Bifunctor (bimap)
import Data.Char (toLower)
import Data.Functor
import qualified Data.Map.Strict  as M
import Data.Maybe
import qualified Data.Text  as T
import qualified Data.Text.IO  as TIO
import Data.Time
import Data.Time.Format.ISO8601
import Hakyll.Core.Identifier
import Hakyll.Core.Metadata
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import qualified Text.Pandoc  as P
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA

pandocMetadata :: Maybe String -> FilePath -> IO Metadata
pandocMetadata mname file = do
  P.Pandoc (P.Meta meta) blocks <- do
    cnt <- TIO.readFile file
    case P.runPure $ P.readOrg P.def cnt of
      Right t -> return t
      Left e -> error $ "Pandoc read failed: " ++ show e
  let furtherMeta =
        M.fromList $
          mapMaybe
            ( \case
                -- Properties at file scope like "#+filetags: TAGS" are not
                -- processed as metadata by the default Pandoc reader. So we
                -- scan through for RawBlocks that match this pattern and read
                -- them as tags here.
                P.RawBlock (P.Format "org") (T.unpack -> text) ->
                  case text =~ ("#\\+([A-Za-z]+):[ \t]+(.+)" :: String) of
                    AllTextSubmatches [_, key, value] ->
                      Just (T.pack key, P.MetaString (T.pack value))
                    _ -> Nothing
                _ -> Nothing
            )
            blocks
      -- The 'Semigroup' operation for 'Map' is 'union', which prefers values
      -- from the left operand.
      metadata = cleanupMetadata mname (meta <> furtherMeta)
  pure $ buildMetadata file (P.Meta metadata)

buildMetadata :: FilePath -> P.Meta -> Metadata
buildMetadata file meta@(P.Meta metadata) =
  AT.fromList $
    map (bimap AT.fromString AT.String) $
      filter (not . T.null . snd) $
        map (\(f, ex, wr) -> (f, inlinesTo wr (ex meta))) $
          [ ("published", publishDate, P.writePlain),
            ("edited", editedDate file, P.writePlain),
            ("route", publishRoute, P.writePlain),
            ("titleHtml", metaField "title", P.writeHtml5String)
          ]
            ++ M.foldMapWithKey
              (\k _ -> [(T.unpack k, metaField k, P.writePlain)])
              metadata

cleanupMetadata ::
  Maybe String ->
  M.Map T.Text P.MetaValue ->
  M.Map T.Text P.MetaValue
cleanupMetadata mname = M.foldMapWithKey ((M.fromList .) . go)
  where
    go "filetags" (P.MetaString value) =
      [ ( "shouldPublish",
          P.MetaBool
            (T.unpack value =~ (":publish=" ++ name ++ ":" :: String))
        )
        | Just name <- [mname]
      ]
        ++ [ ( "tags",
               P.MetaString
                 ( T.intercalate ", "
                     . filter
                       ( \(T.unpack -> s) ->
                           not (s =~ ("^publish=" :: String))
                       )
                     . filter (not . T.null)
                     . T.splitOn ":"
                     $ value
                 )
             )
           ]
    go key value = [(key, value)]

publishRoute :: P.Meta -> [P.Inline]
publishRoute meta =
  datePath
    ++ slugPath
    ++ [P.Str "index.html"]
  where
    slugPath = metaField "slug" meta ++ [P.Str "/"]
    datePath =
      case T.unpack (stringify (publishDate meta))
        =~ ("([0-9]+)-([0-9]+)-" :: String) of
        AllTextSubmatches [_, year, month] ->
          [ P.Str (T.pack year),
            P.Str "/",
            P.Str (T.pack month),
            P.Str "/"
          ]
        _ -> []

getRouteFromMeta :: Metadata -> FilePath
getRouteFromMeta meta =
  case lookupString "route" meta of
    Nothing -> error $ "missing route: " ++ show meta
    Just rte -> rte

publishDateOrDocDate :: P.Meta -> [P.Inline]
publishDateOrDocDate meta =
  case publishDate meta of
    [] -> P.docDate meta
    xs -> xs

publishDate :: P.Meta -> [P.Inline]
publishDate meta =
  case P.lookupMeta "shouldPublish" meta of
    Just (P.MetaBool True) ->
      case metaField "publish" meta of
        [P.Str s] | Just date <- orgDateToIso s -> [P.Str date]
        _ ->
          case metaField "created" meta of
            [P.Str s] | Just date <- orgDateToIso s -> [P.Str date]
            _ -> []
    _ -> []

itemUTC ::
  MonadMetadata m =>
  TimeLocale ->
  Identifier ->
  m (Maybe UTCTime)
itemUTC locale ident =
  getMetadataField ident "published"
    <&> (>>= parseTimeM True locale "%Y-%m-%d")

editedDate :: FilePath -> P.Meta -> [P.Inline]
editedDate file meta =
  case metaField "edited" meta of
    [P.Str s] | Just date <- orgDateToIso s -> [P.Str date]
    _ -> case publishDateOrDocDate meta of
      [] -> unsafePerformIO $ do
        lastModified <- getModificationTime file
        pure [P.Str (T.pack (formatShow iso8601Format (utctDay lastModified)))]
      date -> date

findEntryByUuid :: [Identifier] -> String -> IO (Maybe FilePath)
findEntryByUuid entries (map toLower -> uuid) = do
  firstMatching entries $ \entry -> do
    let path = toFilePath entry
    if takeExtension path == ".org"
      then hasUuid <$> pandocMetadata Nothing path
      else pure Nothing
  where
    -- If this post has the uuid we're looking for, return its route.
    hasUuid :: Metadata -> Maybe FilePath
    hasUuid meta = case lookupString "id" meta of
      Just (map toLower -> postUuid)
        | uuid == postUuid -> lookupString "route" meta
      _ -> Nothing

metaField :: T.Text -> P.Meta -> [P.Inline]
metaField name meta =
  case P.lookupMeta name meta of
    Just (P.MetaString s) -> [P.Str s]
    Just (P.MetaInlines ils) -> ils
    Just (P.MetaBlocks [P.Plain ils]) -> ils
    Just (P.MetaBlocks [P.Para ils]) -> ils
    _ -> []

firstMatching :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
firstMatching [] _ = pure Nothing
firstMatching (x : xs) f =
  f x >>= \case
    Nothing -> firstMatching xs f
    res -> pure res

inlinesTo ::
  (P.WriterOptions -> P.Pandoc -> P.PandocPure T.Text) ->
  [P.Inline] ->
  T.Text
inlinesTo wr ill =
  case P.runPure . wr P.def $ doc of
    Right t -> T.strip t
    Left e -> error $ "Pandoc write failed: " ++ show e
  where
    doc = P.Pandoc P.nullMeta [P.Plain ill]

stringify :: [P.Inline] -> T.Text
stringify = inlinesTo P.writePlain

-- Maybe convert an Org date of form [YYYY-MM-DD WWW( HH:MM)?] to a date of the
-- form YYYY-MM-DD.
orgDateToIso :: T.Text -> Maybe T.Text
orgDateToIso (T.unpack -> date) =
  case date
    =~ ( "\\[([0-9]+)-([0-9]+)-([0-9]+) [A-Za-z]+( [0-9:]+)?\\]" ::
           String
       ) of
    AllTextSubmatches [_, year, month, day, _time] ->
      Just $ T.pack $ mconcat [year, "-", month, "-", day]
    _ -> Nothing
