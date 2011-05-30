module System.FS.RDFS where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse

import qualified System.FS.RDFS.DBUS as DBUS

type HT = ()

rdfsRun :: String -> [String] -> IO ()
rdfsRun pn args = fuseRun pn args rdfsFSOps defaultExceptionHandler

rdfsFSOps :: FuseOperations HT
rdfsFSOps =
    defaultFuseOps { fuseGetFileStat = rdfsGetFileStat
                   , fuseOpen        = rdfsOpen
                   , fuseRead        = rdfsRead
                   , fuseOpenDirectory = rdfsOpenDirectory
                   , fuseReadDirectory = rdfsReadDirectory
                   , fuseGetFileSystemStats = rdfsGetFileSystemStats
                   }
rdfsString :: B.ByteString
rdfsString = B.pack "Rdfs World, HFuse!\n"

rdfsPath :: FilePath
rdfsPath = "/rd"

dbusPath :: FilePath
dbusPath = "/dbus"

dirStat ctx =
    FileStat { statEntryType = Directory
             , statFileMode =
                 foldr1 unionFileModes
                            [ ownerReadMode
                            , ownerExecuteMode
                            , groupReadMode
                            , groupExecuteMode
                            , otherReadMode
                            , otherExecuteMode
                            ]
             , statLinkCount = 2
             , statFileOwner = fuseCtxUserID ctx
             , statFileGroup = fuseCtxGroupID ctx
             , statSpecialDeviceID = 0
             , statFileSize = 4096
             , statBlocks = 1
             , statAccessTime = 0
             , statModificationTime = 0
             , statStatusChangeTime = 0
             }

fileStat ctx =
    FileStat { statEntryType = RegularFile
             , statFileMode =
                 foldr1 unionFileModes
                            [ ownerReadMode
                            , groupReadMode
                            , otherReadMode
                            ]
             , statLinkCount = 1
             , statFileOwner = fuseCtxUserID ctx
             , statFileGroup = fuseCtxGroupID ctx
             , statSpecialDeviceID = 0
             , statFileSize = 0
             , statBlocks = 1
             , statAccessTime = 0
             , statModificationTime = 0
             , statStatusChangeTime = 0
             }

rdfsGetFileStat :: FilePath -> IO (Either Errno FileStat)
rdfsGetFileStat path
    | path == "/" =
        do ctx <- getFuseContext
           return $ Right $ dirStat ctx
    | path == rdfsPath =
        do ctx <- getFuseContext
           return $ Right $ dirStat ctx
    | path == dbusPath =
        do ctx <- getFuseContext
           return $ Right $ dirStat ctx
    -- | otherwise =
    --     do print $ "GetFileStat: " ++ show path
    --        return $ Left eNOENT
    | otherwise =
        do ctx <- getFuseContext
           return $ Right $ dirStat ctx

rdfsOpenDirectory path
    | path == "/"        = return eOK
    | path == dbusPath   = return eOK
    | belongsDbus path  = return eOK
    | otherwise         =
        do print $ "OpenDirectory: " ++ show path
           return eNOENT

belongsDbus :: String -> Bool
belongsDbus p = dbusPath == take (length dbusPath) p

dbusDir :: String -> String
dbusDir = drop (length dbusPath)

rdfsReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
rdfsReadDirectory path
    | path == "/" =
        do ctx <- getFuseContext
           let (_:rdfsName) = rdfsPath
           let (_:dbusName) = dbusPath
           return $ Right [(".",          dirStat  ctx)
                          ,("..",         dirStat  ctx)
                          ,(dbusName,    dirStat ctx)
                          ,(rdfsName,    dirStat ctx)
                          ]
    | path == dbusPath =
        do ctx <- getFuseContext
           dbusServices <- DBUS.listServices
           return $ Right ([(".",          dirStat  ctx)
                           ,("..",         dirStat  ctx)
                           ] ++ [(sn, dirStat ctx) | sn <- dbusServices])
    | belongsDbus path =
        do ctx <- getFuseContext
           dbusEntries <- DBUS.listDir (dbusDir path)
           return $ Right ([(".",          dirStat  ctx)
                           ,("..",         dirStat  ctx)
                           ] ++ [(mn, dirStat ctx) | mn <- dbusEntries])
    | otherwise =
        do print $ "ReadDirectory: " ++ show path
           return (Left eNOENT)

rdfsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
rdfsOpen path mode flags
    | path == rdfsPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         =
        do print $ "Open: " ++ show path
           return (Left eNOENT)


rdfsRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
rdfsRead path _ byteCount offset
    | otherwise         =
        do print $ "Open: " ++ show path
           return $ Left eNOENT

rdfsGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
rdfsGetFileSystemStats str =
    return $ Right FileSystemStats
               { fsStatBlockSize = 512
               , fsStatBlockCount = 1
               , fsStatBlocksFree = 1
               , fsStatBlocksAvailable = 1
               , fsStatFileCount = 5
               , fsStatFilesFree = 10
               , fsStatMaxNameLength = 255
               }
