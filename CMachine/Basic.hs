module CMachine.Basic (initCMaState) where

import CMachine.Core
import CMachine.Syntax
import qualified CMachine.Instructions as CMa

initCMaState = makeCMaState basicDecoder

basicDecoder :: CMa -> Maybe (CMachine c ())
basicDecoder cma = return $ case cma of
      Add         -> CMa.add
      And         -> CMa.and
      Alloc i     -> CMa.alloc i
      Call        -> CMa.call
      Div         -> CMa.div
      Dup         -> CMa.dup
      Enter i     -> CMa.enter i
      Eq          -> CMa.eq
      Geq         -> CMa.geq
      Gr          -> CMa.gr
      Halt        -> CMa.halt
      Jump i      -> CMa.jump i
      JumpZ i     -> CMa.jumpz i
      JumpI i     -> CMa.jumpi i
      Leq         -> CMa.leq
      Le          -> CMa.le
      Load        -> CMa.load
      LoadA i     -> CMa.loada i
      LoadC i     -> CMa.loadc i
      LoadN i     -> CMa.loadn i      -- load with 1 arg
      LoadR i     -> CMa.loadr i
      LoadRC i    -> CMa.loadrc i
      LoadRN i j  -> CMa.loadrn i j   -- loadr with 2 args
      Mark        -> CMa.mark
      Mod         -> CMa.mod
      Mul         -> CMa.mul
      Neg         -> CMa.neg
      Neq         -> CMa.neq
      New         -> CMa.new
      Nop         -> CMa.nop
      Not         -> CMa.not
      Or          -> CMa.or
      Pop         -> CMa.pop_
      Return i    -> CMa.return i
      Slide i j   -> CMa.slide i j
      Store       -> CMa.store
      StoreA i    -> CMa.storea i
      StoreN i    -> CMa.storen i
      StoreR i    -> CMa.storer i
      StoreRN i j -> CMa.storern i j
      Sub         -> CMa.sub
