{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Text.PDF.Info
    (-- * Reading PDF info
     pdfInfo
    ,PDFInfo(..)
    ,PDFSize(..)
    ,PDFInfoError(..)
    -- * Internals
    ,ParsePDFInfo
    ,runParse
    ,parse
    ,parseSize
    ,parseDate
    ,readRight)
    where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception as E
import           Control.Monad.Error

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Prelude
import           System.Exit
import           System.Locale
import           System.Process.Text

-- | A type representing the output from the pdfinfo command.
data PDFInfo = PDFInfo {
    pdfInfoTitle        :: !(Maybe Text)    -- ^ Title
  , pdfInfoSubject      :: !(Maybe Text)    -- ^ Subject
  , pdfInfoAuthor       :: !(Maybe Text)    -- ^ Author: E.g. Chris Done
  , pdfInfoCreator      :: !(Maybe Text)    -- ^ Creator: E.g. Microsoft Office Word 2007
  , pdfInfoProducer     :: !(Maybe Text)    -- ^ Producer: E.g. Microsoft Office Word 2007
  , pdfInfoCreationDate :: !(Maybe UTCTime) -- ^ Creation Date
  , pdfInfoModDate      :: !(Maybe UTCTime) -- ^ Modification Date
  , pdfInfoTagged       :: !(Maybe Bool)    -- ^ Tagged?
  , pdfInfoPages        :: !(Maybe Integer) -- ^ Pages: E.g. 238
  , pdfInfoEncrypted    :: !(Maybe Bool)    -- ^ Encrypted?
  , pdfInfoPageSize     :: !(Maybe PDFSize) -- ^ Page: E.g. 595.32 x 841.92 pts (A4)
  , pdfInfoFileSize     :: !(Maybe Integer) -- ^ File: E.g. 4061737 bytes
  , pdfInfoOptimized    :: !(Maybe Bool)    -- ^ Optimized?
  , pdfInfoPDFVersion   :: !(Maybe Double)  -- ^ PDF: E.g. 1.5
  } deriving Show

-- | Possible things that can go wrong while reading the info.
data PDFInfoError
  = ParseError !String        -- ^ Couldn't parse a property value.
  | ProcessFailure !Text      -- ^ Process exited with this stderr.
  | ProcessError !IOException -- ^ Error to do with the pdfinfo process.
  | NoMessage                 -- ^ No message given.
  | SomeError String          -- ^ Some nonspecific error.
  deriving Show

-- | Size of the PDF in pts.
data PDFSize = PDFSize { pdfSizeW :: !Float, pdfSizeH :: !Float }
  deriving (Eq,Show)

instance Error PDFInfoError where noMsg = NoMessage; strMsg = SomeError
newtype ParsePDFInfo a = ParsePDFInfo { runParse :: Either PDFInfoError a }
  deriving (Monad,Functor,MonadError PDFInfoError)
instance Applicative ParsePDFInfo where (<*>) = ap; pure = return

-- | Run pdfinfo on the given file. Handles IO exceptions to do with
-- running the process.
pdfInfo :: MonadIO m => FilePath -> m (Either PDFInfoError PDFInfo)
pdfInfo path = liftIO $ loadInfo `E.catch` ioErrorHandler where
  loadInfo = do (code,out,err) <- readProcessWithExitCode "pdfinfo" ["-enc","UTF-8",path] ""
                case code of
                  ExitSuccess -> return (parse out)
                  ExitFailure{} -> return (Left (ProcessFailure err))
  ioErrorHandler = return . Left . ProcessError

-- | Parse PDFInfo's output.
parse :: Text -> Either PDFInfoError PDFInfo
parse out = runParse $
  PDFInfo <$> string "Title"
          <*> string "Subject"
          <*> string "Author"
          <*> string "Creator"
          <*> string "Producer"
          <*> date "CreationDate"
          <*> date "ModDate"
          <*> bool "Tagged"
          <*> integer "Pages"
          <*> bool "Encrypted"
          <*> size "Page size"
          <*> integer "File size"
          <*> bool "Optimized"
          <*> floating "PDF version"
    where string = get id
          date = get (>>= parseDate)
          size = get (>>= parseSize)
          bool = get $ fmap $ \yes -> yes == "yes"
          floating = readIt
          integer = readIt
          readIt :: Read a => Text -> ParsePDFInfo (Maybe a)
          readIt = get (>>= readRight)
          properties = map split . T.lines $ out
          get f name =
            case lookup name properties of
              Just ok -> catchError (Just <$> (f $ return $ T.strip ok))
                                    (\_ -> return Nothing)
              Nothing -> return Nothing

          split = second (T.drop 2) . T.span (/=':')

-- | Parse a page size. This is loosely defined.
parseSize :: Text -> ParsePDFInfo PDFSize
parseSize s =
  case T.words s of
    ((readRight -> Right x):"x":(readRight -> Right y):_) ->
        return $ PDFSize x y
    _ -> throwError $ ParseError $ "Unable to read size: " ++ show s

-- | Parse a date according to pdfinfo's format.
parseDate :: Text -> ParsePDFInfo UTCTime
parseDate s =
  case parseTime defaultTimeLocale "%a %b %e %H:%M:%S %Y" (T.unpack s) of
    Just ok -> return ok
    Nothing -> throwError $ ParseError $ "Unable to parse date: " ++ show s

-- | Read a value, maybe, allow misc trailing data.
readRight :: (MonadError PDFInfoError m,Read a) => Text -> m a
readRight s =
  case reads (T.unpack s) of
    [(v,_)] -> return v
    _ -> throwError $ ParseError $ "Couldn't read value: " ++ show s
