module CMachine.Syntax where

--
-- These are all the instructions for the C machine described in Chapter 2.
--
data CMa = Add
         | And
         | Alloc Int
         | Call
         | Div
         | Dup
         | Enter Int
         | Eq
         | Geq
         | Gr
         | Halt
         | Jump Int
         | JumpZ Int
         | JumpI Int -- jump SP[param]
         | Leq
         | Le
         | Load      -- like LoadA, but the address is TOS.
         | LoadN Int -- in the book this is "load m," i.e., load with a param
         | LoadA Int -- push 
         | LoadC Int -- push constant param
         | LoadR Int -- push FP[param]
         | LoadRC Int -- push FP + param
         | LoadRN Int Int -- push FP[param0..param0+param1]
         | Mark
         | Mod
         | Mul
         | Neg
         | Neq
         | New
         | Nop       -- ext instruction
         | Not       -- ext instruction
         | Or
         | Pop
         | Return Int
         | Slide Int Int
         | Store
         | StoreA Int
         | StoreN Int
         | StoreR Int
         | StoreRN Int Int
         | Sub
             
instance Show CMa where             
      show (Add)          = "add"
      show (And)          = "and"
      show (Alloc i)      = "alloc " ++ show i
      show (Call)         = "call"
      show (Div)          = "div"
      show (Dup)          = "dup"
      show (Enter i)      = "enter " ++ show i
      show (Eq)           = "eq"
      show (Geq)          = "geq"
      show (Gr)           = "gr"
      show (Halt)         = "halt"
      show (Jump i)       = "jump " ++ show i
      show (JumpZ i)      = "jumpz " ++ show i
      show (JumpI i)      = "jumpi " ++ show i
      show (Leq)          = "leq"
      show (Le)           = "le"
      show (Load)         = "load"
      show (LoadA i)      = "loada " ++ show i
      show (LoadC i)      = "loadc " ++ show i
      show (LoadN i)      = "load " ++ show i
      show (LoadR i)      = "loadr " ++ show i
      show (LoadRC i)     = "loadrc " ++ show i
      show (LoadRN i j)   = "loadr " ++ show i ++ " " ++ show j
      show (Mark)         = "mark"
      show (Mod)          = "mod"
      show (Mul)          = "mul"
      show (Neg)          = "neg"
      show (Neq)          = "neq"
      show (New)          = "new"
      show (Nop)          = "nop"
      show (Not)          = "not"
      show (Or)           = "or"
      show (Pop)          = "pop"
      show (Return i)     = "return " ++ show i
      show (Slide i j)    = "slide " ++ show i ++ " " ++ show j
      show (Store)        = "store"
      show (StoreA i)     = "storea " ++ show i
      show (StoreN i)     = "storen" ++ show i
      show (StoreR i)     = "storer " ++ show i
      show (StoreRN i j)  = "storer " ++ show i ++ " " ++ show j
      show (Sub)          = "sub"

