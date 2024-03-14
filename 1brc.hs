import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe(fromJust)

main :: IO ()
main = do
  str <- B.readFile "measurements.txt"
  let parseInt = fst . fromJust . B.readInt . (B.filter (/='.')) . B.tail
      ls = map ((\(a,b)-> (a, parseInt b)) . B.break (== ';')) (B.lines str)
      f (a,b,c,d) (a',b',c',d') = let (i,j,k,l) = (min a a', max b b', c+c', d+d') in
        i `seq` j `seq` k `seq` l `seq` (i,j,k,l)
      collect = foldl (\m (k,v) -> M.insertWith f (B.toStrict k) (v,v,v,1) m) M.empty ls
  mapM_ (putStrLn . show) $ M.mapWithKey
    (\k (a,b,c,d)-> (B.unpack (B.fromStrict k),
                     fromIntegral a/10,fromIntegral b/10,(fromIntegral c)/(10*d))) collect
