--
-- These are the CMa instructions given in ch. 2 of Wilhelm & Seidl.
--

module CMachine.Instructions 
      ( add, and, alloc
      , call, div, dup
      , enter, eq
      , geq, gr
      , halt
      , jump, jumpz, jumpi
      , leq, le
      , load, loada, loadc, loadn, loadr, loadrn, loadrc
      , mark, mod, mul
      , neg, neq, new, nop, not
      , or, pop_, return 
      , slide
      , store, storea, storen, storer, storern
      , sub
      ) where

import Foreign.Marshal.Utils (fromBool, toBool)
import Control.Applicative ((<$>))
import Prelude hiding
      ( and, or, not
      , div, mod
      , return
      ) 
import qualified Prelude as P

import CMachine.Core

-- Not exported.
pushb :: Bool -> CMachine c ()
pushb = push . fromBool

-- Not exported.
popb :: CMachine c Bool
popb = toBool <$> pop

add :: CMachine c ()
add = do
      b <- pop
      a <- pop
      push (a+b)

and :: CMachine c ()
and = do
      b <- popb
      a <- popb
      pushb $ a && b

alloc :: Int -> CMachine c ()
alloc m = do
      sp <- getr SP
      putr SP $ sp + m

call :: CMachine c ()
call = do
      getr SP >>= putr FP
      pc <- getr PC
      pop >>= putr PC
      push pc

div :: CMachine c ()
div = do
      b <- pop
      a <- pop
      push (a `P.div` b)

dup :: CMachine c ()
dup = do
      v <- pop
      push v
      push v

enter :: Int -> CMachine c ()
enter m = do
      sp <- getr SP
      putr EP $ sp + m

      ep <- getr EP
      hp <- getr HP
      if ep >= hp
            then stackOverflow
            else nop

eq :: CMachine c ()
eq = do
      b <- pop
      a <- pop
      pushb $ a==b

geq :: CMachine c ()
geq = le >> not

gr :: CMachine c ()
gr = leq >> not

jumpz :: Int -> CMachine c ()
jumpz l = do
   b <- pop
   if b == 0
         then jump l
         else nop

jumpi :: Int -> CMachine c ()
jumpi b = do
      q <- pop
      jump $ b + q

leq :: CMachine c ()
leq = do
      b <- pop
      a <- pop
      pushb $ a <= b

le :: CMachine c ()
le = do
      b <- pop
      a <- pop
      pushb $ a < b

load :: CMachine c ()
load = do
      i <- pop
      v <- get i
      push v

loada :: Int -> CMachine c ()
loada i = loadc i >> load

loadc :: Int -> CMachine c ()
loadc = push 

loadn :: Int -> CMachine c ()
loadn m = do
      p <- pop
      loadn' m p
loadn' :: Int -> Int -> CMachine c ()
loadn' 0 p = nop
loadn' m p = do
      v <- get p
      push v
      loadn' (m - 1) (p + 1)

loadr :: Int -> CMachine c ()
loadr j = loadrc j >> load

loadrn :: Int -> Int -> CMachine c ()
loadrn j m = loadrc j >> loadn m

loadrc :: Int -> CMachine c ()
loadrc j = do
      fp <- getr FP
      push $ fp + j

mark :: CMachine c ()
mark = do
      ep <- getr EP
      fp <- getr FP
      push ep
      push fp

mod :: CMachine c ()
mod = do
      b <- pop
      a <- pop
      push $ a `P.mod` b

mul :: CMachine c ()
mul = do
      b <- pop
      a <- pop
      push $ a * b

neg :: CMachine c ()
neg = do
      a <- pop
      push (-a)

neq :: CMachine c ()
neq = eq >> not

new :: CMachine c ()
new = do
      n <- pop
      hp <- getr HP
      ep <- getr EP
      if hp - n > ep
            then putr HP (hp - n) >> push (hp - n)
            else push 0

nop :: CMachine c ()
nop = P.return ()

not :: CMachine c ()
not = push 0 >> eq

or :: CMachine c ()
or = do
      b <- popb
      a <- popb
      pushb $ a || b

pop_ :: CMachine c ()
pop_ = pop >> nop

return :: Int -> CMachine c ()
return q = do
      fp <- getr FP
      hp <- getr HP
      pc' <- get fp
      fp' <- get $ fp - 1
      ep' <- get $ fp - 2
      putr PC pc'
      putr EP ep'
      if ep' >= hp
            then stackOverflow
            else putr SP (fp - q) >> putr FP fp'

slide :: Int -> Int -> CMachine c ()
slide 0 _ = nop
slide q m = do
      sp <- getr SP
      putr SP $ sp - q - m
      slide' q m
slide' :: Int -> Int -> CMachine c ()
slide' q 0 = nop
slide' q m = do
      sp <- getr SP
      loada $ sp + q + 1
      slide' q $ m - 1

store :: CMachine c ()
store = do
      i <- pop
      sp <- getr SP
      v <- get sp
      put (i, v)

storen :: Int -> CMachine c ()
storen m = do
      sp <- getr SP
      to <- pop
      storen' (sp - m) to m
storen' :: Int -> Int -> Int -> CMachine c ()
storen' _ _ 0 = nop
storen' from to m = do
      v <- get from
      put (to, v)
      storen' (from+1) (to+1) (m-1)

storea :: Int -> CMachine c ()
storea i = loadc i >> store

storer :: Int -> CMachine c ()
storer j = loadrc j >> store

storern :: Int -> Int -> CMachine c ()
storern j m = loadrc j >> storen m

sub :: CMachine c ()
sub = do
      b <- pop
      a <- pop
      push $ a - b

