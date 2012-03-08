{-# LANGUAGE ForeignFunctionInterface, TypeOperators, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, TypeOperators #-}
module TACCompiler( 
  compileProgram,
  compileProgram',
  buildCompileState,
  writeCodeGenModuleOpt,
  GenState(..)
  )
       where

import Prelude hiding (sin, exp, tan, tanh, max, min)

import Data.Word
--import Data.TypeLevel(d0, d1, d2, D10)

--import qualified LLVM.FFI.Core
--import qualified LLVM.FFI
import LLVM.Core
--import LLVM.Util.File
import LLVM.Util.Loop(Phi(..))
import LLVM.Util.Optimize
--import LLVM.ExecutionEngine

import qualified TAC
import TAC(maxRegister, Instruction(..), Operation(..), prog1, Program(..))

import qualified Data.Map as Map

import Control.Monad.State

data GenState = GenState {
  regSet :: [Value (Ptr Double)],
  funcExp :: Function (Double -> IO Double),
  funcSin :: Function (Double -> IO Double),
  funcTan :: Function (Double -> IO Double),
  funcTanh :: Function (Double -> IO Double),
  funcMax :: Function (Double -> Double -> IO Double),
  funcMin :: Function (Double -> Double -> IO Double),
  funcLogistic :: Function (Double -> IO Double),
  funcWriteSample :: Function (Double -> IO ())
  }

type FunctionGen r a = StateT GenState (CodeGenFunction r) a

evalFunctionGen ss s = evalStateT s (ss {regSet = []})

allocateRegisters :: FunctionGen r () -- CodeGenFunction r [Value (Ptr Double)]
allocateRegisters = do
  regs <- lift $ mapM (const $ do 
                          r <- alloca
                          store (valueOf 0.0) r 
                          return r) [1..maxRegister]
  modify (\s -> s {regSet = regs})

loadReg ::  Int -> FunctionGen r (Value Double) 
loadReg n = do
  regs <- gets regSet
  lift $ load (regs !! n)

storeReg ::  (Value Double) -> Int -> FunctionGen r () 
storeReg v n = do
  regs <- gets regSet
  lift $ store v (regs !! n)

emitOperation :: Operation -> (Value Double) -> (Value Double) -> FunctionGen r (Value Double)
emitOperation X x _ = return x
emitOperation Y _ y = return y
emitOperation Mult x y = lift $ mul x y
emitOperation Add x y = lift $ add x y
emitOperation Mod x y = lift $ frem x y
emitOperation Sin x y = do 
  t <- lift $ mul x y
  sin <- gets funcSin
  lift $ call sin t
emitOperation Tan x y = do 
  t <- lift $ mul x y
  tan <- gets funcTan
  lift $ call tan t
emitOperation Tanh x y = do 
  t <- lift $ mul x y
  tanh <- gets funcTanh
  lift $ call tanh t
emitOperation Max x y = do 
  max <- gets funcMax
  lift $ call max x y
emitOperation Min x y = do 
  min <- gets funcMin
  lift $ call min x y
emitOperation LogisticFunc x _ = do 
  logistic <- gets funcLogistic
  lift $ call logistic x

{-               | Max -- max x y
               | Min -- min x y
               | LogisticFunc -- 1/(1+e^(-x)), ignores y
-}

loadTACValue :: TAC.RegOrVal -> FunctionGen r (Value Double)
loadTACValue (TAC.Register r) = loadReg r
loadTACValue (TAC.Value v) = return (valueOf v)

emitInstruction (Instruction op x y accum z) = do
  x' <- loadTACValue x
  y' <- loadTACValue y
  t <- emitOperation op x' y'
  z' <- loadReg z
  r <- emitOperation accum z' t
  storeReg r z

-- Loop the index variable from low to high.  The state in the loop starts as start, and is modified
-- by incr in each iteration.
forLoop :: forall i a r . (Phi a, Num i, IsConst i, IsInteger i, IsFirstClass i, CmpRet i Bool) =>
           Value i -> Value i -> a -> (Value i -> a -> FunctionGen r a) -> FunctionGen r a
forLoop low high start incr = do
    top <- lift getCurrentBasicBlock
    loop <- lift newBasicBlock
    body <- lift newBasicBlock
    exit <- lift newBasicBlock

    lift $ br loop

    lift $ defineBasicBlock loop
    i <- lift $ phi [(low, top)]
    vars <- lift $ phis top start
    t <- lift $ cmp CmpNE i high
    lift $ condBr t body exit

    lift $ defineBasicBlock body

    vars' <- incr i vars
    i' <- lift $ add i (valueOf 1 :: Value i)

    body' <- lift $ getCurrentBasicBlock
    
    lift $ do
      addPhis body' vars vars'
      addPhiInputs i [(i', body')]
      br loop
      defineBasicBlock exit
      
    return vars
    
mTest2 :: CodeGenModule (Function (Word32 -> IO ()))
mTest2 = do
  --s <- buildCompileState
  compileProgram prog1

compileProgram :: Program -> CodeGenModule (Function (Word32 -> IO ()))
compileProgram prog = do
  s <- buildCompileState
  compileProgram' prog s

buildCompileState :: CodeGenModule GenState
buildCompileState = do
  exp <- newNamedFunction ExternalLinkage "llvm.exp.f64" :: TFunction (Double -> IO Double)
  sin <- newNamedFunction ExternalLinkage "llvm.sin.f64" :: TFunction (Double -> IO Double)
  tan <- newNamedFunction ExternalLinkage "llvm.tan.f64" :: TFunction (Double -> IO Double)
  tanh <- newNamedFunction ExternalLinkage "tanh" :: TFunction (Double -> IO Double)
  logistic <- newNamedFunction ExternalLinkage "logistic" :: TFunction (Double -> IO Double)
  --addAttributes logistic [ReadNoneAttribute, NoUnwindAttribute]
  max <- newNamedFunction ExternalLinkage "tac_max" :: TFunction (Double -> Double -> IO Double)
  --addAttributes max [ReadNoneAttribute, NoUnwindAttribute]
  min <- newNamedFunction ExternalLinkage "tac_min" :: TFunction (Double -> Double -> IO Double)
  --addAttributes min [ReadNoneAttribute, NoUnwindAttribute]
  writeSample <- newNamedFunction ExternalLinkage "writeSample" :: TFunction (Double -> IO ())
  
  return GenState {funcSin = sin, funcTan = tan, funcTanh = tanh, funcExp = exp, 
                    funcLogistic = logistic, funcMax = max, funcMin = min,
                    regSet = [], funcWriteSample = writeSample}

compileProgram' :: Program -> GenState -> CodeGenModule (Function (Word32 -> IO ()))
compileProgram' prog s = do
  let Program outreg instrs = prog
      GenState {funcWriteSample = writeSample } = s

  createNamedFunction ExternalLinkage "generate" $ \ n -> evalFunctionGen s $ do
    allocateRegisters
      {-top <- lift $ getCurrentBasicBlock
    loop <- lift $ newBasicBlock
    end <- lift $ newBasicBlock
    lift $ br loop
    
    lift $ defineBasicBlock loop-}
    _ <- forLoop (valueOf 0) n () $ \ _ _ -> do 
      -- Store time
      time <- loadReg TAC.regTime
      -- Emit instructions
      mapM_ emitInstruction instrs
      -- Restore fixed registers
      let fixed = TAC.fixedRegisters 0
      mapM_ (\(r, v)-> storeReg (valueOf v) r) (Map.toList fixed)
      -- Update time
      time' <- lift $ add time (valueOf (1.0 :: Double))
      storeReg time' TAC.regTime
      -- Output sample
      out <- loadReg outreg
      lift $ call writeSample out 
      return ()
      
      --defineBasicBlock end
    lift $ ret ()
  
writeCodeGenModuleOpt :: FilePath -> CodeGenModule a -> IO ()
writeCodeGenModuleOpt name f = do
    m <- newModule
    _ <- defineModule m f
    _ <- optimizeModule 3 m
    writeBitcodeToFile name m


main :: IO ()
main = do
    initializeNativeTarget
    writeCodeGenModuleOpt "TACTest.bc" mTest2
    return ()
