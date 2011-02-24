{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module Text.PDF.Info 
    (-- * Reading PDF info
     pdfInfo
    ,PDFInfo(..)
    ,PDFSize(..)
    ,PDFInfoError(..)
    -- * Internals
    ,parse
    ,parseSize
    ,parseDate
    ,readRight)
    where

import Prelude hiding (catch)
import Control.Monad.Error
import System.Process
import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Time
import System.Locale
import Control.Exception

-- | A type representing the output from the pdfinfo command.
data PDFInfo = PDFInfo {
    pdfInfoAuthor       :: Maybe String  -- ^ Author: E.g. Chris Done
  , pdfInfoCreator      :: Maybe String  -- ^ Creator: E.g. Microsoft® Office Word 2007
  , pdfInfoProducer     :: Maybe String  -- ^ Producer: E.g. Microsoft® Office Word 2007
  , pdfInfoCreationDate :: Maybe UTCTime -- ^ Creation Date
  , pdfInfoModDate      :: Maybe UTCTime -- ^ Modification Date
  , pdfInfoTagged       :: Maybe Bool    -- ^ Tagged?
  , pdfInfoPages        :: Maybe Integer -- ^ Pages: E.g. 238
  , pdfInfoEncrypted    :: Maybe Bool    -- ^ Encrypted?
  , pdfInfoPageSize     :: Maybe PDFSize -- ^ Page: E.g. 595.32 x 841.92 pts (A4)
  , pdfInfoFileSize     :: Maybe Integer -- ^ File: E.g. 4061737 bytes
  , pdfInfoOptimized    :: Maybe Bool    -- ^ Optimized?
  , pdfInfoPDFVersion   :: Maybe Double  -- ^ PDF: E.g. 1.5
  } deriving Show

-- | Possible things that can go wrong while reading the info.
data PDFInfoError = 
    ParseError String        -- ^ Couldn't parse a property value.
  | ProcessError IOException -- ^ Error to do with the pdfinfo process.
  | NoMessage                -- ^ No message given.
  | SomeError String         -- ^ Some nonspecific error.
  deriving Show

-- | Size of the PDF in pts.
data PDFSize = PDFSize { pdfSizeW :: Float, pdfSizeH :: Float }
  deriving (Eq,Show)

instance Error PDFInfoError where noMsg = NoMessage; strMsg = SomeError
newtype ParsePDFInfo a = ParsePDFInfo { runParse :: Either PDFInfoError a }
  deriving (Monad,Functor,MonadError PDFInfoError)
instance Applicative ParsePDFInfo where (<*>) = ap; pure = return

-- | Run pdfinfo on the given file. Handles IO exceptions to do with
-- running the process.
pdfInfo :: MonadIO m => FilePath -> m (Either PDFInfoError PDFInfo)
pdfInfo path = liftIO $ loadInfo `catch` ioErrorHandler where
  loadInfo = parse <$> readProcess "pdfinfo" [path] ""
  ioErrorHandler = return . Left . ProcessError

-- | Parse PDFInfo's output.
parse :: String -> Either PDFInfoError PDFInfo
parse out = runParse $
  PDFInfo <$> string "Author"
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
          readIt :: Read a => String -> ParsePDFInfo (Maybe a)
          readIt = get (>>= readRight)

          properties = map split . lines $ out
          get f name =
            case lookup name properties of
              Just ok -> catchError (Just <$> (f $ return $ trim ok))
                                    (\_ -> return Nothing)
              Nothing -> return Nothing

          split = second (drop 2) . span (/=':')
          trim = bi reverse (dropWhile isSpace) . dropWhile isSpace

-- | Parse a page size. This is loosely defined.
parseSize :: String -> ParsePDFInfo PDFSize
parseSize s = 
  case words s of
    ((readRight -> Right x):"x":(readRight -> Right y):_) -> 
        return $ PDFSize x y
    _ -> throwError $ ParseError $ "Unable to read size: " ++ show s

-- | Parse a date according to pdfinfo's format.
parseDate :: String -> ParsePDFInfo UTCTime
parseDate s = 
  case parseTime defaultTimeLocale "%a %b %d %H:%M:%S %Y" s of
    Just ok -> return ok
    Nothing -> throwError $ ParseError $ "Unable to parse date: " ++ show s

-- | Read a value, maybe, allow misc trailing data.
readRight :: (MonadError PDFInfoError m,Read a) => String -> m a
readRight s = 
  case reads s of 
    [(v,_)] -> return v
    _ -> throwError $ ParseError $ "Couldn't read value: " ++ show s

-- | Untwist a value, apply a function, twist it back.
bi :: (c1 -> c) -> (c -> c1) -> c1 -> c
bi g f = g . f . g
