module TAC where

import Control.Monad.Random

import Control.Monad.State
import Data.Map(Map)
import qualified Data.Map as Map

type Register = Int

maxRegister = 64

type Value = Double

data RegOrVal = Register Register
              | Value Value
                deriving (Eq, Read, Ord)

instance Show RegOrVal where
  show (Register r) = "r" ++ show r
  show (Value v) = show v

-- Fixed registers
regTime = 0
regPi = 1
regZero = 2
regOne = 3

fixedRegisters :: Value -> Map Register Value
fixedRegisters t = Map.fromList [
  (regTime, t), -- Time
  (regPi, pi), -- Pi
  (regZero, 0), -- Zero
  (regOne, 1) -- One
  ]


{-
  An Operation is a full instuction in the form:
    op1 x y op2 z
  This will perform the operation:
    z = z op2 (x op1 y)

  
-}
data Instruction = Instruction Operation RegOrVal RegOrVal Operation Register
                   deriving (Eq, Read, Ord)
                            
instance Show Instruction where
  show (Instruction op x y accum z) = (show x) ++ " `" ++ (show op) ++ "` " ++ (show y) ++ 
                                      " --" ++ (show accum) ++ "-> r" ++ (show z)

data Operation = X  -- Just return X
               | Y  -- Just return y
               | Mult -- x * y
               | Add -- x + y
               | Max -- max x y
               | Min -- min x y
               | Mod -- x 'mod' y
               | LogisticFunc -- 1/(1+e^(-x)), ignores y
               | Tanh -- tanh(x), ignores y
               | Sin -- sin(x*y)
               | Tan -- tan(x*y)
                 deriving (Eq, Show, Read, Ord)

type ExecMonad = State (Map Register Value)

type OperationImpl = Value -> Value -> Value
type InstructionImpl = ExecMonad ()
type ProgramImpl = ExecMonad Value

-- Instructions and the output register. The output register is run once per
-- execution of the instructions.
data Program = Program Register [Instruction]
             deriving (Eq, Read, Ord)
                      
instance Show Program where
  show (Program out is) = (foldl (\acc i -> acc ++ "  " ++ (show i) ++ "\n") "{\n" is) ++ 
                          "} outputing r" ++ (show out) ++ "\n"

operationImpl :: Operation -> OperationImpl
operationImpl X = curry fst
operationImpl Y = curry snd
operationImpl Mult = (*)
operationImpl Add = (+)
operationImpl Max = max
operationImpl Min = min
operationImpl Mod = \x y -> x - ((fromIntegral $ floor $ x / y) * y)
operationImpl LogisticFunc = \x _-> 1/(1 + 2.7**x)
operationImpl Tanh = \x _-> tanh x
operationImpl Sin = \x y -> sin (x*y)
operationImpl Tan = \x y -> tan (x*y)

getReg r = gets (Map.findWithDefault 0 r)
setReg r v = modify (Map.insert r v)
getValue (Value v) = return v
getValue (Register r) = getReg r

instuctionImpl :: Instruction -> InstructionImpl
instuctionImpl (Instruction op x y accum z) =
  if z `Map.notMember` (fixedRegisters 0) then do
    x' <- getValue x
    y' <- getValue y
    let a = operationImpl op x' y'
    z' <- getReg z
    let b = operationImpl accum z' a
    setReg z b
  else
    return ()
    

programImpl :: Program -> ProgramImpl
programImpl (Program outreg prog) = 
  let prog' = map instuctionImpl prog in
  do
    t <- getReg regTime
    sequence_ prog'
    modify (Map.union (fixedRegisters (t+1)))
    getReg outreg
  
evalProgram :: Int -> Program -> [Value]
evalProgram n prog = evalState (sequence $ replicate n impl) Map.empty
  where impl = programImpl prog

prog1 = Program 10 [
    Instruction Sin (Register regTime) (Value 0.1) Y 10,
    Instruction Sin (Register regTime) (Value 0.15) Add 10,
    Instruction X (Register regTime) (Value 1) Y 0,   
    Instruction Min (Register regTime) (Register 10) Y 10    
  ]

randomFromList l g = (l !! n, g')
  where (n, g') = randomR (0, length l-1) g

instance Random Operation where
  random g = randomFromList [X  -- Just return X
               , Y  -- Just return y
               , Mult -- x * y
               , Add -- x + y
               , Max -- max x y
               , Min -- min x y
               , Mod -- x 'mod' y
               , LogisticFunc -- 1/(1+e^(-x)), ignores y
               , Tanh -- tanh(x), ignores y
               , Sin -- sin(x*y)
               , Tan -- tan(x*y)
               ] g
             
             
randomReg :: (RandomGen g) => Rand g Register
randomReg = getRandomR (0, maxRegister)
randomVal :: (RandomGen g) => Rand g Value
randomVal = getRandomR (-1, 1)
             
instance Random RegOrVal where 
  random g = runRand act g
    where act = do
            b <- getRandom
            if b then (do
              r <- randomReg
              return (Register r))
              else do
              v <- randomVal
              return $ Value v
             

instance Random Instruction where 
  random g = runRand act g
    where act = do
            op <- getRandom 
            accum <- getRandom
            x <- getRandom
            y <- getRandom
            z <- randomReg
            return $ Instruction op x y accum z
                
              
randomOutReg :: (RandomGen g) => Rand g Register
randomOutReg = getRandomR (16, maxRegister)
              
instance Random Program where
  random g = runRand act g
    where act = do
            outreg <- randomOutReg
            n <- getRandomR (10, 20)
            l <- sequence (replicate n getRandom)
            return $ Program outreg l
  