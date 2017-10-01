
import Data.ByteString.Lazy as L (take, empty, append, drop, fromStrict, readFile, ByteString)
import Data.ByteString      as B (take, ByteString)
import Data.Word                 (Word8, Word16, Word32, Word64)
import Data.Binary.Get           (skip, getWord8, getWord16le, getWord32le, getWord64le, getByteString, isEmpty, runGet, Get)
import Data.List                 (head, tail, last, init, null, reverse, takeWhile)
import Data.Text.Encoding        (decodeUtf16LE)
import Data.Text                 (unpack, Text)
import Data.UUID                 (fromByteString, UUID)
import System.Environment        (getArgs)

-- 共通系関数 ------------------------------------------------------------------------------------

prints :: Show a => [a] -> IO ()

prints [] = do
    return ()

prints (a:as) = do
    print a
    prints as

getList :: Get a -> Int -> Get [a]
getList g i = do
    if i == 0
        then return []
        else 
            do 
                item    <- g
                items   <- getList g (i - 1)
                return ( item : items )

getWord32leList =  
    getList getWord32le

getList2 :: Get a -> Get [a]
getList2 g = 
    do
        empty <- isEmpty
        if empty
        then 
            return []
        else 
            do 
                a <- g
                as <- getList2 g
                return (a:as)

getWord32leList2 =  
    getList2 getWord32le

-- CompoundFileHeader ----------------------------------------------------------------------------

data CompoundFileHeader = CompoundFileHeader
    {
        headerSignature              :: !B.ByteString,
        headerCLSID                  :: !(Maybe UUID),
        minorVersion                 :: !Word16,
        majorVersion                 :: !Word16,
        byteOrder                    :: !Word16,
        sectorShift                  :: !Word16,
        miniSectorShift              :: !Word16,
        reserved                     :: !B.ByteString,
        numberofDirectorySectors     :: !Word32,
        numberofFATSectors           :: !Word32,
        firstDirectorySectorLocation :: !Word32,
        transactionSignatureNumber   :: !Word32,
        miniStreamCutoffSize         :: !Word32,
        firstMiniFATSectorLocation   :: !Word32,
        numberofMiniFATSectors       :: !Word32,
        firstDIFATSectorLocation     :: !Word32,
        numberofDIFATSectors         :: !Word32,
        difats                       :: ![Word32]
    } deriving (Show)

getCompoundFileHeader :: L.ByteString -> CompoundFileHeader
getCompoundFileHeader allFileData = 

    let get = do
        headerSignature              <- getByteString 8
        headerCLSID                  <- getByteString 16
        minorVersion                 <- getWord16le
        majorVersion                 <- getWord16le
        byteOrder                    <- getWord16le
        sectorShift                  <- getWord16le
        miniSectorShift              <- getWord16le
        reserved                     <- getByteString 6
        numberofDirectorySectors     <- getWord32le
        numberofFATSectors           <- getWord32le
        firstDirectorySectorLocation <- getWord32le
        transactionSignatureNumber   <- getWord32le
        miniStreamCutoffSize         <- getWord32le
        firstMiniFATSectorLocation   <- getWord32le
        numberofMiniFATSectors       <- getWord32le
        firstDIFATSectorLocation     <- getWord32le
        numberofDIFATSectors         <- getWord32le
        difats                       <- getWord32leList 109

        return $! CompoundFileHeader 
            headerSignature 
            (fromByteString $ fromStrict headerCLSID)
            minorVersion 
            majorVersion 
            byteOrder 
            sectorShift 
            miniSectorShift 
            reserved 
            numberofDirectorySectors 
            numberofFATSectors 
            firstDirectorySectorLocation 
            transactionSignatureNumber 
            miniStreamCutoffSize 
            firstMiniFATSectorLocation 
            numberofMiniFATSectors 
            firstDIFATSectorLocation 
            numberofDIFATSectors 
            difats

    in  runGet get allFileData


-- DirectoryEntry --------------------------------------------------------------------------------

data DirectoryEntry = DirectoryEntry
    {
        directoryEntryName          :: !Text,
        directoryEntryNameLength    :: !Word16,
        objectType                  :: !Word8,
        colorFlag                   :: !Word8,
        leftSiblingID               :: !Word32,
        rightSiblingID              :: !Word32,
        childID                     :: !Word32,
        clsid                       :: !(Maybe UUID),
        stateBits                   :: !Word32,
        creationTime                :: !Word64,
        modifiedTime                :: !Word64,
        startingSectorLocation      :: !Word32,
        streamSize                  :: !Word64
    } deriving (Show)

getDirectoryEntry :: Get DirectoryEntry
getDirectoryEntry = do

    directoryEntryName          <- getByteString 64
    directoryEntryNameLength    <- getWord16le
    objectType                  <- getWord8
    colorFlag                   <- getWord8
    leftSiblingID               <- getWord32le
    rightSiblingID              <- getWord32le
    childID                     <- getWord32le
    clsid                       <- getByteString 16 
    stateBits                   <- getWord32le
    creationTime                <- getWord64le
    modifiedTime                <- getWord64le
    startingSectorLocation      <- getWord32le
    streamSize                  <- getWord64le

    return $! DirectoryEntry
        (decodeUtf16LE  $ B.take (fromIntegral directoryEntryNameLength - 2) directoryEntryName)
        directoryEntryNameLength
        objectType
        colorFlag
        leftSiblingID
        rightSiblingID
        childID
        (fromByteString $ fromStrict clsid)
        stateBits
        creationTime
        modifiedTime
        startingSectorLocation
        streamSize

getAllDirectoryEntries :: L.ByteString -> [DirectoryEntry]
getAllDirectoryEntries allFileData = 

    let header = getCompoundFileHeader allFileData
        stream = getStreamData allFileData $ firstDirectorySectorLocation header
    in runGet ( getList2 getDirectoryEntry ) stream

siblingDirectoryEntryIds :: [DirectoryEntry] -> Word32 -> [Word32]
siblingDirectoryEntryIds allDirectoryEntries index =
    if 0xFFFFFFFF == index 
    then
        []
    else
        let current = allDirectoryEntries !! (fromIntegral index)
            left = siblingDirectoryEntryIds allDirectoryEntries (leftSiblingID current)
            right = siblingDirectoryEntryIds allDirectoryEntries (rightSiblingID current)
        in left ++ ( index : right)

printDirectoryEntries :: (Word32 -> Text) -> (Word32 -> [Word32]) -> [Bool] -> Word32 -> IO ()
printDirectoryEntries  p children lines currentid = do

    if null lines
    then 
        return ()
    else
        do
            mapM_   ( \f -> putStr  $ if f then "　" else "┃" ) 
                    (reverse $ tail lines)

            putStr $ if head lines then "┗ " else "┣ "

    putStrLn $ unpack  $ p currentid

    if null $ children currentid 
    then
        return ()
    else
        do
            let c = children currentid
                f = printDirectoryEntries  p children (False:lines) 
                initc = init c
            mapM_ f initc
            printDirectoryEntries  p children (True:lines) (last $ children currentid)


-- DIFAT -----------------------------------------------------------------------------------------

sectorDifats :: L.ByteString -> Word32 -> [Word32]
sectorDifats allFileData sectorIndex =
    if 0xFFFFFFFF == sectorIndex
    then
        []
    else
        let sectorData = getSectorData allFileData sectorIndex
            difats = runGet getWord32leList2 sectorData
            nextdifats = sectorDifats allFileData (last difats)
        in (init difats) ++ nextdifats

allDifats :: L.ByteString -> [Word32]
allDifats allFileData =
    let header = getCompoundFileHeader allFileData
        headerDifats = difats header
        firstSectorIndex = firstDIFATSectorLocation header
        serctorDifats = sectorDifats allFileData firstSectorIndex
        compositeDifats = headerDifats ++ serctorDifats
    in takeWhile ( < 0xFFFFFFFF ) compositeDifats


-- FAT -------------------------------------------------------------------------------------------

getFATsFromOneDifat :: L.ByteString -> Word32 -> [Word32]
getFATsFromOneDifat allFileData difat = 
    let get = do
        skip ( 512 * (1 + fromIntegral difat) )
        getWord32leList 128
    in  runGet get allFileData

getFATs :: L.ByteString -> [Word32]
getFATs allFileData =
    let header = getCompoundFileHeader allFileData
    in  (allDifats allFileData) >>= 
        (   \difat -> 
                if (difat < 0xFFFFFFFF) 
                then 
                    ( getFATsFromOneDifat allFileData difat ) 
                else 
                [] 
        )


-- MiniFAT ---------------------------------------------------------------------------------------

getMiniFATs :: L.ByteString -> [Word32]
getMiniFATs allSectorData = 
    let header = getCompoundFileHeader allSectorData
        stream = getStreamData allSectorData $ firstMiniFATSectorLocation header
    in runGet getWord32leList2 stream

getMiniFATStream :: L.ByteString -> L.ByteString
getMiniFATStream allFileData = 
    let allDirectoryEntries = getAllDirectoryEntries allFileData
        rootDirectoryEntry = head allDirectoryEntries
        s = startingSectorLocation rootDirectoryEntry
    in getStreamData allFileData s


-- GetSectorData ---------------------------------------------------------------------------------

getSectorData :: L.ByteString -> Word32 -> L.ByteString
getSectorData allFileData sector =
    let data1 = L.drop ( 512 * (1 + fromIntegral sector)) allFileData
        data2 = L.take 512 data1
    in data2


-- Stream ----------------------------------------------------------------------------------------

getStreamData :: L.ByteString -> Word32 -> L.ByteString
getStreamData allFileData firstSector =
    
    let fat = getFATs allFileData
        firstdata = getSectorData allFileData firstSector
        next = fat !! fromIntegral firstSector
        nextdata = 
            if  next < 0xFFFFFFFB
                then 
                    getStreamData allFileData next
                else
                    L.empty

    in L.append firstdata nextdata


-- Main ------------------------------------------------------------------------------------------

main = do 

    args <- getArgs

    allFileData <- L.readFile $ head args

    let allDirectoryEntries = 
            getAllDirectoryEntries allFileData

    let childIds id = 
            siblingDirectoryEntryIds allDirectoryEntries $ childID $ allDirectoryEntries !! fromIntegral id

    printDirectoryEntries 
        (directoryEntryName.(allDirectoryEntries !!).fromIntegral) 
        childIds
        [] 
        0 
