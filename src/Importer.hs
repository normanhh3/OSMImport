 {- # LANGUAGE OverloadedStrings # -}

module Importer where

import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (forever, when)
import Data.Binary.Get (Get, getWord32be, getLazyByteString, runGet, bytesRead)
import Codec.Compression.Zlib (decompress)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as BL (readFile, length, ByteString, fromStrict)
import Data.List.Split (splitOn)
import qualified Data.Foldable as F(toList)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.ProtocolBuffers (decodeMessage, getField)
import Common(nano,deltaDecode,calculateDegrees)
import qualified Data.Node as N
import qualified Data.Tag as T
import qualified Data.Way as W
import qualified Database as MDB (saveNodes,saveWays,saveRelation)
import Data.OSMFormat
import Data.Maybe (fromJust, isJust)
import qualified Data.Serialize as S (runGetLazy, runGet)
import qualified Data.Text as TXT (unpack) 
import qualified Data.ByteString.Char8 as BCE (unpack)


data Chunk = Chunk {blob_header :: BlobHeader, blob :: Blob} deriving (Show)

getChunks :: Integral a => a -> a -> [Chunk] -> Get [Chunk]
getChunks limit location chunks
  | limit > location = do
    len <- getWord32be
    headerBytes <- getLazyByteString (fromIntegral len)
    let Right blobHeader = S.runGetLazy decodeMessage =<< Right headerBytes :: Either String BlobHeader
    blobData <- getLazyByteString $ fromIntegral $ getField $ bh_datasize blobHeader
    let Right blob = S.runGetLazy decodeMessage =<< Right blobData :: Either String Blob
    bytesRead' <- bytesRead
    let location' = fromIntegral bytesRead' 
    getChunks limit location' ((Chunk blobHeader blob) : chunks)
  | otherwise = return $ reverse chunks


startImport :: String -> String -> String -> IO ()
startImport dbconnection dbname filename = do
  let host = (splitOn ":" dbconnection) !! 0
  let port = read ((splitOn ":" dbconnection) !! 1) :: Int
  let dbNodecommand recs = MDB.saveNodes dbconnection dbname recs
  performImport filename dbNodecommand 


performImport :: FilePath -> ([N.ImportNode] -> IO ()) -> IO ()
performImport fileName dbNodecommand = do
  handle <- BL.readFile fileName
  let fileLength = fromIntegral $ BL.length handle
  let chunks = runGet (getChunks fileLength (0 :: Integer) []) handle
  putStrLn $ "File Length : [" ++ (show fileLength) ++ "] Contains: [" ++ (show (length chunks)) ++ "] chunks"
  processData chunks 1
    where
      processData [] _ = return ()
      processData (x:xs) count = do
        let blobUncompressed = decompress $ BL.fromStrict . fromJust . getField $ b_zlib_data (blob x)
        let btype = getField $ bh_type (blob_header x)
        case (TXT.unpack btype) of
          "OSMHeader" -> do
            let Right headerBlock = S.runGetLazy decodeMessage =<< Right blobUncompressed :: Either String HeaderBlock
            let b = fromJust . getField $ hb_bbox headerBlock 
            let minlat = (fromIntegral $ getField $ hbbox_bottom b) / nano
            let minlon = (fromIntegral $ getField $ hbbox_left b) / nano
            let maxlat = (fromIntegral $ getField $ hbbox_top b) / nano
            let maxlon = (fromIntegral $ getField $ hbbox_right b) / nano
            putStrLn $ "Bounding Box (lat, lon): (" ++ (show minlat) ++ "," ++ (show minlon) ++ ") (" ++ (show maxlat) ++ "," ++ (show maxlon) ++ ")"
            putStrLn $ "Chunk : [" ++ (show count) ++ "] Header data"
            processData xs (count + 1)
          "OSMData" -> do
            let primitiveBlock = S.runGetLazy decodeMessage =<< Right blobUncompressed :: Either String PrimitiveBlock
            case (primitiveBlock) of
              Right pblock -> do 
                          let st = map BCE.unpack . getField $ st_bytes (getField $ pb_stringtable pblock)
                          let gran = fromIntegral . fromJust . getField $ pb_granularity pblock 
                          let pg = getField $ pb_primitivegroup pblock 
                          _ <- primitiveGroups pg st gran
                          putStrLn $ "Chunk : [" ++ (show count) ++ "] OSMData"
              Left errorString -> print errorString
            processData xs (count + 1)

      -- Primitive Groups
      primitiveGroups [] _ _ = return ()
      primitiveGroups (x:xs) st gran = do
        let pgNodes =  getField $ pg_dense x
        when (isJust pgNodes) (dbNodecommand . denseNodes $ fromJust pgNodes)
        primitiveGroups xs st gran
      
      -- let pgWays = getField $ pg_ways x
      --   let pgRelations = F.toList $ getVal x relations
        
        -- let impWays = parseImpWays pgWays
      --   let impRelations = parseImpRelations pgRelations
        -- dbWaycommand impWays
        -- dbRelationcommand impRelations

        -- let newCount = count + (length impNodes) + (length impWays) + (length impRelations)
        -- let newCount = count + (length impNodes)
        -- primitiveGroups xs st gran 1
        where
          denseNodes :: DenseNodes -> [N.ImportNode]
          denseNodes d = do 
            let ids = map fromIntegral $ getField $ dense_nodes_id d
            let latitudes = map fromIntegral $ getField $ dense_nodes_lat d
            let longitudes = map fromIntegral $ getField $ dense_nodes_lon d
            let keyvals = map fromIntegral $ getField $ dense_nodes_keys_vals d

            let maybeInfo = getField $ dense_nodes_info d
            case maybeInfo of
              Just info -> do 
                let versions = map fromIntegral (getField $ dense_info_version info) 
                let timestamps = map fromIntegral (getField $ dense_info_timestamp info) 
                let changesets = map fromIntegral (getField $ dense_info_changeset info) 
                let uids = map fromIntegral (getField $ dense_info_uid info) 
                let sids = map fromIntegral (getField $ dense_info_user_sid info) 
                buildNodeData ids latitudes longitudes keyvals versions timestamps changesets uids sids
              Nothing -> buildNodeData ids latitudes longitudes keyvals [] [] [] [] []

          buildNodeData :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [N.ImportNode]
          buildNodeData ids lat lon keyvals versions timestamps changesets uids sids = do
            let identifiers = deltaDecode ids 0
            let latitudes = calculateDegrees (deltaDecode lat 0) gran
            let longitudes = calculateDegrees (deltaDecode lon 0) gran
            let decodedTimestamps = deltaDecode timestamps 0
            let decodedChangesets = deltaDecode changesets 0
            let decodedUIDs = deltaDecode uids 0
            let decodedUsers = deltaDecode sids 0
            
            buildNodes identifiers latitudes longitudes keyvals versions decodedTimestamps decodedChangesets decodedUIDs decodedUsers

          buildNodes :: [Integer] -> [Float] -> [Float] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [N.ImportNode]
          buildNodes [] [] [] [] [] [] [] [] [] = []
          buildNodes (id:ids) (lat:lats) (long:longs) keyvals (ver:versions) (ts:timestamps) (cs:changesets) (uid:uids) (sid:sids) = 
                          N.ImportNodeFull {  N._id=id
                                            , N.latitude=lat
                                            , N.longitude=long
                                            , N.tags=(fst $ lookupMixedKeyVals keyvals)
                                            , N.version=ver
                                            , N.timestamp=ts
                                            , N.changeset=cs
                                            , N.uid=uid
                                            , N.sid=(st !! (fromIntegral sid :: Int))
                                          } : buildNodes ids lats longs (snd $ lookupMixedKeyVals keyvals) versions timestamps changesets uids sids
          buildNodes (id:ids) (lat:lats) (long:longs) keyvals [] [] [] [] [] =
                          N.ImportNodeSmall id lat long (fst $ lookupMixedKeyVals keyvals) : buildNodes ids lats longs (snd $ lookupMixedKeyVals keyvals) [] [] [] [] []
            

          lookupMixedKeyVals :: [Integer] -> ([T.ImportTag], [Integer])
          lookupMixedKeyVals keyvals= splitKeyVal keyvals []
            where
              splitKeyVal :: [Integer] -> [T.ImportTag] -> ([T.ImportTag], [Integer])
              splitKeyVal [] [] = ([], [])
              splitKeyVal [] y = (y, [])
              splitKeyVal (x:xx:xs) y 
                | x == 0 = (y, (xx : xs))
                | otherwise = splitKeyVal xs (T.ImportTag (st !! (fromIntegral x :: Int)) (st !! (fromIntegral xx :: Int)) : y)
              splitKeyVal (x:_) y = (y, []) -- In the case that the array is on an unequal number, Can happen if the last couple of entries are 0


          lookupKeyVals :: [Int] -> [Int] -> [T.ImportTag]
          lookupKeyVals [] [] = []
          lookupKeyVals (x:xs) (y:ys) = do
            T.ImportTag (st !! x) (st !! y) : lookupKeyVals xs ys


          -- parseImpWays :: [Way] -> [W.ImportWay]
          -- parseImpWays [] = []
          -- parseImpWays (x:xs) = do
          --   buildImpWay x : parseImpWays xs
          --   where
          --     buildImpWay :: Way -> W.ImportWay
          --     buildImpWay pgWay = do
          --       let id = fromIntegral $ getField $ way_id pgWay
          --       let keys = map fromIntegral $ getField $ way_keys pgWay
          --       let vals = map fromIntegral $ getField $ way_vals pgWay
          --       let refs = map fromIntegral $ getField $ way_refs pgWay
          --       let info = fromJust $ getField $ way_info pgWay
          --       let deltaRefs = deltaDecode refs 0
          --       W.ImportWay { W._id=id
          --                   , W.tags=(lookupKeyVals keys vals)
          --                   , W.version=fromIntegral . fromJust . getField $ info_version info
          --                   , W.timestamp=fromIntegral . fromJust . getField $ info_timestamp info
          --                   , W.changeset=fromIntegral . fromJust . getField $ info_changeset info
          --                   , W.uid=fromIntegral . fromJust . getField $ info_uid info
          --                   , W.user=(st !! (fromIntegral . fromJust . getField $ info_user_sid info :: Int))
          --                   , W.nodes=deltaRefs}



      --     parseImpRelations :: [Relation] -> [R.ImportRelation]
      --     parseImpRelations [] = []
      --     parseImpRelations (x:xs) = do
      --       buildImpRelation x : parseImpRelations xs
      --       where
      --         buildImpRelation :: Relation -> R.ImportRelation
      --         buildImpRelation pgRelation = do
      --           let id = fromIntegral (getVal pgRelation OSM.OSMFormat.Relation.id)
      --           let keys = map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.keys)
      --           let vals = map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.vals)
      --           let info = (getVal pgRelation OSM.OSMFormat.Relation.info)
      --           let types = map show $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.types)
      --           let memids = deltaDecode (map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.memids)) 0
      --           R.ImportRelation { R._id=id
      --                       , R.tags=(lookupKeyVals keys vals)
      --                       , R.version=fromIntegral (getVal info OSM.OSMFormat.Info.version)
      --                       , R.timestamp=fromIntegral (getVal info OSM.OSMFormat.Info.timestamp)
      --                       , R.changeset=fromIntegral (getVal info OSM.OSMFormat.Info.changeset)
      --                       , R.user=(st !! (fromIntegral (getVal info OSM.OSMFormat.Info.user_sid) :: Int))
      --                       , R.members=buildRelationTags types memids
      --                     }
      --             where
      --               buildRelationTags :: [String] -> [Int] -> [ImportTag]
      --               buildRelationTags [] [] = []
      --               buildRelationTags (x:xs) (y:ys) = ImportTag x (show y) : buildRelationTags xs ys



showUsage :: IO ()
showUsage = do
      hPutStrLn stderr "usage: dbconnection dbname filename"
      hPutStrLn stderr "example: OSMImport '127.0.0.1:27017' 'geo_data' './download/england-latest.osm.pbf'"
      exitFailure

