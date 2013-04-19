module CMachine.Core
      ( CMaState, status
      , makeCMaState
      , dumpData, dumpDataSwatch, dumpCode, dumpStack, showRegs
      , clear, clearCode, clearData, resizeData
      , CMaStatus (..)
      , Reg (..)
      , getr, putr
      , CMachine (..)
      , fetch, pushi, get, put, top, pop, push, jump
      , halt, stackOverflow
      , step, execute
      ) where

-- We use arrays to represent the instruction and data stores.
import Data.Array.IArray
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Applicative

import CMachine.Syntax

type DataSeg  = Array Int Int
type CodeSeg c = Array Int c
            
--- CMaState ---
 
data CMaState c = CMaState 
      { dataSeg :: DataSeg
      , codeSeg :: CodeSeg c
      , regs :: Regs
      , status :: CMaStatus
      , decoder :: c -> Maybe (CMachine c ())
      }

instance Show c => Show (CMaState c) where
      show s = showRegs s ++ 
            show (dumpStack s) ++ "\n" ++ 
            show (dumpData s) ++ "\n" ++
            show (dumpCode s)

makeCMaState :: (c -> Maybe (CMachine c ())) -> Int -> CMaState c
makeCMaState decoder dataSegSize = 
      CMaState (array (0, maxAddr) [(i, 0) | i <- [0..maxAddr]]) 
            (array (0,-1) [])
            (Regs 0 0 maxAddr 0 0) 
            OK
            decoder
                  where maxAddr = dataSegSize - 1

showRegs :: CMaState c -> String
showRegs s = show (regs s) ++
            ", status = " ++ show (status s)

dumpData :: CMaState c -> [Int]
dumpData s = elems (dataSeg s)

-- dumpDataSwatch s i n dumps n cells at offset i.
dumpDataSwatch :: CMaState c -> Int -> Int -> [Int]
dumpDataSwatch s i n = map (dataSeg s!) [(max minAddr i)..(min (n+i-1) maxAddr)]
      where maxAddr = snd $ bounds $ dataSeg s
            minAddr = fst $ bounds $ dataSeg s


dumpCode :: CMaState c -> [c]
dumpCode s = elems (codeSeg s)

dumpStack :: CMaState c -> [Int]
dumpStack s = 
      map (dataSeg s!) [0 .. sp']
            where sp' = if sp (regs s) < snd (bounds $ dataSeg s)
                  then sp $ regs s
                  else snd $ bounds $ dataSeg s

dumpHeap :: CMaState c -> [Int]
dumpHeap s = 
      map (dataSeg s!) [hp .. max]
            where hp = sp $ regs s
                  max = snd $ bounds $ dataSeg s

resizeData :: Int -> CMaState c -> CMaState c
resizeData sz' s = s 
      { dataSeg = listArray (0, maxAddr) els'
      , regs = Regs pc' sp' hp' fp' ep'
      } where 
            maxAddr = sz' - 1
            sz = snd $ bounds $ dataSeg s
            els = elems $ dataSeg s
            els' = if sz' <= sz
                  then take sz' els
                  else els ++ take (sz' - sz) zeros
            zeros = 0:zeros
            -- Clearly resizing the data segment can screw all the regs up.
            pc' = pc $ regs s
            sp' = min (sp $ regs s) maxAddr
            hp' = min (hp $ regs s) maxAddr
            fp' = min (fp $ regs s) maxAddr
            ep' = min (ep $ regs s) maxAddr


clearData :: CMaState c -> CMaState c
clearData s = s 
      { dataSeg = amap (\_ -> 0) $ dataSeg s
      , regs = Regs 0 0 (snd $ bounds $ dataSeg s) 0 0
      , status = OK -- TODO
      }

clearCode :: CMaState c -> CMaState c
clearCode s = s 
      { codeSeg = array (0, -1) [] 
      , regs = (regs s) { pc = 0}
      }

clear :: CMaState c -> CMaState c
clear = clearCode . clearData

--- CMaStatus ---

data CMaStatus = OK | Running 
               | DecodeFail 
               | SegFaultC | SegFaultD
               | StackOverflow
               | Halted
      deriving (Show, Eq)

getStatus = CMachine $ \s -> (s, Just $ status s)
setStatus status' = CMachine $ \s -> (s {status=status'}, Just ())

--- Registers ---

data Reg = PC | SP | HP | FP | EP
      deriving (Show)

data Regs = Regs {pc::Int, sp::Int, hp::Int, fp::Int, ep::Int}

instance Show (Regs) where
      show rs = 
            "PC = " ++ show (pc rs) ++ 
            ", SP = " ++ show (sp rs) ++ 
            ", HP = " ++ show (hp rs) ++ 
            ", FP = " ++ show (fp rs) ++ 
            ", EP = " ++ show (ep rs)

getr :: Reg -> CMachine c Int
getr PC = CMachine $ \s -> (s, Just $ pc $ regs s)
getr SP = CMachine $ \s -> (s, Just $ sp $ regs s)
getr HP = CMachine $ \s -> (s, Just $ hp $ regs s)
getr FP = CMachine $ \s -> (s, Just $ fp $ regs s)
getr EP = CMachine $ \s -> (s, Just $ ep $ regs s)

putr :: Reg -> Int -> CMachine c ()
putr PC v = CMachine $ \s -> (s {regs=(regs s) {pc=v}}, Just ())
putr SP v = CMachine $ \s -> (s {regs=(regs s) {sp=v}}, Just ())
putr HP v = CMachine $ \s -> (s {regs=(regs s) {hp=v}}, Just ())
putr FP v = CMachine $ \s -> (s {regs=(regs s) {fp=v}}, Just ())
putr EP v = CMachine $ \s -> (s {regs=(regs s) {ep=v}}, Just ())

getDecoder = CMachine $ \s -> (s, Just $ decoder s)

--- CMachine ---

newtype CMachine c a = CMachine {runCMachine :: CMaState c -> (CMaState c, Maybe a)}

instance Monad (CMachine c) where
      m >>= k = CMachine $ \s -> let (s', r) = (runCMachine m s) in
            case r of
                  Just a -> runCMachine (k a) s'
                  Nothing -> (s', Nothing)

      return a = CMachine $ \s -> (s, Just a)

instance Functor (CMachine c) where
      fmap f m = f <$> m

--- Memory ---

die :: CMachine c ()
die = CMachine $ \s -> (s, Nothing)

getData :: CMachine c (Array Int Int)
getData = CMachine $ \s -> (s, Just $ dataSeg s)

setData :: Array Int Int -> CMachine c ()
setData dataSeg' = CMachine $ \s -> (s {dataSeg=dataSeg'}, Just ())

getCode :: CMachine c (Array Int c)
getCode = CMachine $ \s -> (s, Just $ codeSeg s)

setCode :: Array Int c -> CMachine c ()
setCode codeSeg' = CMachine $ \s -> (s {codeSeg=codeSeg'}, Just ())

checkBoundsC :: Int -> CMachine c ()
checkBoundsC i = do
      (min, max) <- bounds <$> getCode
      when (i < min || i > max) segFaultC

checkBoundsD :: Int -> CMachine c ()
checkBoundsD i = do
      (min, max) <- bounds <$> getData
      when (i < min || i > max) segFaultD

--
-- Our CMa "microcode."
--
-- Fetch the next instruction for execution (and increment PC).
fetch :: CMachine c c
fetch = do
      pc <- getr PC
      codeSeg <- getCode
      checkBoundsC pc
      putr PC $ pc + 1
      return $ codeSeg!pc

-- Store an encoded instruction into the code segment.
pushi :: c -> CMachine c ()
pushi c = do
      codeSeg <- getCode
      setCode $ listArray (0, snd (bounds codeSeg) + 1) $ elems codeSeg ++ [c]

get :: Int -> CMachine c Int
get i = do
      checkBoundsD i
      dataSeg <- getData
      return $ dataSeg!i

put :: (Int, Int) -> CMachine c ()
put (i, v) = do
      checkBoundsD i
      dataSeg <- getData
      setData $ dataSeg//[(i, v)]

top :: CMachine c Int
top = getr SP >>= get 

pop :: CMachine c Int
pop = do
      sp <- getr SP
      v <- get sp
      putr SP $ sp - 1
      return v

push :: Int -> CMachine c ()
push v = do
      sp <- getr SP
      put (sp + 1, v)
      putr SP $ sp + 1

jump :: Int -> CMachine c ()
jump = putr PC

halt :: CMachine c ()
halt = setStatus Halted >> die

segFaultC :: CMachine c ()
segFaultC = setStatus SegFaultC >> die

segFaultD :: CMachine c ()
segFaultD = setStatus SegFaultD >> die

stackOverflow :: CMachine c ()
stackOverflow = setStatus StackOverflow >> die

decode :: c -> CMachine c ()
decode c = do
      d <- getDecoder
      fromMaybe (setStatus DecodeFail) (d c)

step :: CMachine c ()
step = fetch >>= decode

execute :: CMachine c ()
execute = fetch >>= decode >> execute

