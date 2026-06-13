{- cabal:
build-depends: base, binary, bytestring, text, uuid
-}

-- comview: 複合ファイル (MS-CFB) のディレクトリ構造をツリー表示するツール
-- 仕様: https://learn.microsoft.com/openspecs/windows_protocols/ms-cfb/
module Main (main) where

import           Control.Monad        (replicateM)
import           Data.Binary.Get      (Get, getByteString, getWord16le,
                                       getWord32le, getWord64le, getWord8,
                                       isEmpty, runGet)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Int             (Int64)
import           Data.Text            (Text, unpack)
import           Data.Text.Encoding   (decodeUtf16LE)
import           Data.UUID            (UUID, fromByteString)
import           Data.Word            (Word16, Word32, Word64, Word8)
import           System.Environment   (getArgs)
import           System.Exit          (die)

-- 定数 (MS-CFB 2.1, 2.2) -------------------------------------------------------------------------

-- セクタサイズ。バージョン 3 (sectorShift = 9) の 512 バイト固定
sectorSize :: Int64
sectorSize = 512

-- これ以下の値だけが実セクタ番号。それ以外は DIFSECT/FATSECT/ENDOFCHAIN/FREESECT などの特殊値
maxRegularSector :: Word32
maxRegularSector = 0xFFFFFFFA

isRegularSector :: Word32 -> Bool
isRegularSector = (<= maxRegularSector)

-- FREESECT (未使用 DIFAT エントリ) と NOSTREAM (ディレクトリ ID なし) は同じ値
freeSector, noStream :: Word32
freeSector = 0xFFFFFFFF
noStream   = 0xFFFFFFFF

-- 共通パーサ --------------------------------------------------------------------------------------

-- 入力を使い切るまで g を繰り返す
getUntilEmpty :: Get a -> Get [a]
getUntilEmpty g = do
    done <- isEmpty
    if done
        then return []
        else (:) <$> g <*> getUntilEmpty g

getAllWord32le :: Get [Word32]
getAllWord32le = getUntilEmpty getWord32le

getMaybeUUID :: Get (Maybe UUID)
getMaybeUUID = fromByteString . L.fromStrict <$> getByteString 16

-- CompoundFileHeader ------------------------------------------------------------------------------

data CompoundFileHeader = CompoundFileHeader
    { headerSignature              :: !B.ByteString
    , headerCLSID                  :: !(Maybe UUID)
    , minorVersion                 :: !Word16
    , majorVersion                 :: !Word16
    , byteOrder                    :: !Word16
    , sectorShift                  :: !Word16
    , miniSectorShift              :: !Word16
    , reserved                     :: !B.ByteString
    , numberOfDirectorySectors     :: !Word32
    , numberOfFATSectors           :: !Word32
    , firstDirectorySectorLocation :: !Word32
    , transactionSignatureNumber   :: !Word32
    , miniStreamCutoffSize         :: !Word32
    , firstMiniFATSectorLocation   :: !Word32
    , numberOfMiniFATSectors       :: !Word32
    , firstDIFATSectorLocation     :: !Word32
    , numberOfDIFATSectors         :: !Word32
    , headerDIFAT                  :: ![Word32]
    } deriving (Show)

getCompoundFileHeader :: Get CompoundFileHeader
getCompoundFileHeader = CompoundFileHeader
    <$> getByteString 8         -- headerSignature
    <*> getMaybeUUID            -- headerCLSID
    <*> getWord16le             -- minorVersion
    <*> getWord16le             -- majorVersion
    <*> getWord16le             -- byteOrder
    <*> getWord16le             -- sectorShift
    <*> getWord16le             -- miniSectorShift
    <*> getByteString 6         -- reserved
    <*> getWord32le             -- numberOfDirectorySectors
    <*> getWord32le             -- numberOfFATSectors
    <*> getWord32le             -- firstDirectorySectorLocation
    <*> getWord32le             -- transactionSignatureNumber
    <*> getWord32le             -- miniStreamCutoffSize
    <*> getWord32le             -- firstMiniFATSectorLocation
    <*> getWord32le             -- numberOfMiniFATSectors
    <*> getWord32le             -- firstDIFATSectorLocation
    <*> getWord32le             -- numberOfDIFATSectors
    <*> replicateM 109 getWord32le  -- headerDIFAT

-- DirectoryEntry ----------------------------------------------------------------------------------

data DirectoryEntry = DirectoryEntry
    { directoryEntryName       :: !Text
    , directoryEntryNameLength :: !Word16
    , objectType               :: !Word8
    , colorFlag                :: !Word8
    , leftSiblingID            :: !Word32
    , rightSiblingID           :: !Word32
    , childID                  :: !Word32
    , clsid                    :: !(Maybe UUID)
    , stateBits                :: !Word32
    , creationTime             :: !Word64
    , modifiedTime             :: !Word64
    , startingSectorLocation   :: !Word32
    , streamSize               :: !Word64
    } deriving (Show)

getDirectoryEntry :: Get DirectoryEntry
getDirectoryEntry = do
    rawName    <- getByteString 64
    nameLength <- getWord16le
    DirectoryEntry (decodeEntryName rawName nameLength) nameLength
        <$> getWord8        -- objectType
        <*> getWord8        -- colorFlag
        <*> getWord32le     -- leftSiblingID
        <*> getWord32le     -- rightSiblingID
        <*> getWord32le     -- childID
        <*> getMaybeUUID    -- clsid
        <*> getWord32le     -- stateBits
        <*> getWord64le     -- creationTime
        <*> getWord64le     -- modifiedTime
        <*> getWord32le     -- startingSectorLocation
        <*> getWord64le     -- streamSize

-- 名前は UTF-16LE。長さは終端 NUL の 2 バイトを含む
decodeEntryName :: B.ByteString -> Word16 -> Text
decodeEntryName raw len = decodeUtf16LE $ B.take (fromIntegral len - 2) raw

getAllDirectoryEntries :: L.ByteString -> [Word32] -> CompoundFileHeader -> [DirectoryEntry]
getAllDirectoryEntries fileData fat header =
    runGet (getUntilEmpty getDirectoryEntry) $
        getStreamData fileData fat (firstDirectorySectorLocation header)

-- left/right sibling の木を中順にたどり、同じ階層のエントリ ID を名前順で列挙する
siblingDirectoryEntryIds :: [DirectoryEntry] -> Word32 -> [Word32]
siblingDirectoryEntryIds entries entryId
    | entryId == noStream = []
    | otherwise           = left ++ entryId : right
  where
    entry = entries !! fromIntegral entryId
    left  = siblingDirectoryEntryIds entries (leftSiblingID entry)
    right = siblingDirectoryEntryIds entries (rightSiblingID entry)

-- DIFAT -------------------------------------------------------------------------------------------

-- ヘッダに収まらない分の DIFAT。各 DIFAT セクタの末尾エントリは次の DIFAT セクタ番号
sectorDIFAT :: L.ByteString -> Word32 -> [Word32]
sectorDIFAT fileData sector
    | isRegularSector sector = init entries ++ sectorDIFAT fileData (last entries)
    | otherwise              = []
  where
    entries = runGet getAllWord32le (getSectorData fileData sector)

allDIFAT :: L.ByteString -> CompoundFileHeader -> [Word32]
allDIFAT fileData header =
    takeWhile (/= freeSector) $
        headerDIFAT header ++ sectorDIFAT fileData (firstDIFATSectorLocation header)

-- FAT ---------------------------------------------------------------------------------------------

-- DIFAT が指す全 FAT セクタを連結した、ファイル全体の FAT
getFATs :: L.ByteString -> CompoundFileHeader -> [Word32]
getFATs fileData header =
    allDIFAT fileData header >>= runGet getAllWord32le . getSectorData fileData

-- セクタ / ストリーム読み出し ----------------------------------------------------------------------

-- セクタ番号は本体 (ヘッダの直後) からの通し番号
getSectorData :: L.ByteString -> Word32 -> L.ByteString
getSectorData fileData sector =
    L.take sectorSize $ L.drop (sectorSize * (1 + fromIntegral sector)) fileData

-- FAT のチェーンをたどってストリーム全体を読む
getStreamData :: L.ByteString -> [Word32] -> Word32 -> L.ByteString
getStreamData fileData fat = go
  where
    go sector
        | isRegularSector sector =
            getSectorData fileData sector `L.append` go (fat !! fromIntegral sector)
        | otherwise = L.empty

-- ツリー表示 ----------------------------------------------------------------------------------------

-- isLastFlags は各祖先階層で「最後の子か」のフラグ (直近の階層が先頭、ルートでは空)
printDirectoryTree :: (Word32 -> Text) -> (Word32 -> [Word32]) -> [Bool] -> Word32 -> IO ()
printDirectoryTree nameOf childrenOf isLastFlags entryId = do
    putStrLn $ branchPrefix isLastFlags ++ unpack (nameOf entryId)
    case childrenOf entryId of
        [] -> return ()
        cs -> do
            mapM_ (printDirectoryTree nameOf childrenOf (False : isLastFlags)) (init cs)
            printDirectoryTree nameOf childrenOf (True : isLastFlags) (last cs)

branchPrefix :: [Bool] -> String
branchPrefix []                  = ""
branchPrefix (isLast : parents)  =
    concatMap (\lastChild -> if lastChild then "　" else "┃") (reverse parents)
        ++ if isLast then "┗ " else "┣ "

-- Main ----------------------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> printCompoundFile =<< L.readFile path
        _      -> die "usage: comview <compound file>"

printCompoundFile :: L.ByteString -> IO ()
printCompoundFile fileData =
    let header     = runGet getCompoundFileHeader fileData
        fat        = getFATs fileData header
        entries    = getAllDirectoryEntries fileData fat header
        entryAt    = (entries !!) . fromIntegral
        nameOf     = directoryEntryName . entryAt
        childrenOf = siblingDirectoryEntryIds entries . childID . entryAt
    in printDirectoryTree nameOf childrenOf [] 0
