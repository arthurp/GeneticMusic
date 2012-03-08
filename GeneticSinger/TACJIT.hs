module TACJIT where

import qualified TAC
import TACCompiler

import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Optimize

import Foreign.Ptr
import Data.IORef

import Control.Applicative

foreign import ccall "wrapper"
  mkFuncDD_D :: (Double -> Double -> IO Double) -> IO (FunPtr (Double -> Double -> IO Double))
foreign import ccall "wrapper"
  mkFuncD_D :: (Double -> IO Double) -> IO (FunPtr (Double -> IO Double))
foreign import ccall "wrapper"
  mkFuncD_V :: (Double -> IO ()) -> IO (FunPtr (Double -> IO ()))

foreign import ccall "math.h &tan"
   tanFuncPtr :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &tanh"
   tanhFuncPtr :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &sin"
   sinFuncPtr :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &exp"
   expFuncPtr :: FunPtr (Double -> IO Double)
             
makeModuleFromGen :: CodeGenModule a -> IO (Module, a)
makeModuleFromGen f = do
    m <- newModule
    r <- defineModule m f
    _ <- optimizeModule 3 m
    return (m, r)

logisticIO :: Double -> IO Double
logisticIO x = return $ 1/(1 + 2.7**x)
maxIO :: Double -> Double -> IO Double
maxIO x y = return $ max x y
minIO :: Double -> Double -> IO Double
minIO x y = return $ min x y

runProgram :: Int -> TAC.Program -> IO [Double]
runProgram n prog = do
  initializeNativeTarget
  
  (m, (s, genFunc)) <- makeModuleFromGen $ do
    s <- buildCompileState
    f <- compileProgram' prog s
    return (s, f)
    
  writeBitcodeToFile "/tmp/tmp.bc" m

  
  let GenState {funcSin = sinFunc, funcTan = tanFunc, funcTanh = tanhFunc, funcExp = expFunc, 
                funcLogistic = logisticFunc, funcMax = maxFunc, funcMin = minFunc,
                funcWriteSample = writeSampleFunc} = s
  logisticFuncPtr <- mkFuncD_D logisticIO
  maxFuncPtr <- mkFuncDD_D (maxIO)
  minFuncPtr <- mkFuncDD_D (minIO)
  
  listRef <- newIORef []
  
  writeSampleFuncPtr <- mkFuncD_V (\s -> do
                                      o <- readIORef listRef
                                      writeIORef listRef (s : o))
  funcsUsed <- getFunctions m
  --print funcsUsed
  --print [show logisticFunc, show maxFunc, show minFunc, show writeSampleFunc]
  generate <- runEngineAccess $ do
    addModule m
    let af s a b = case lookup s funcsUsed of
          Just _ -> addFunctionValue a b
          Nothing -> return ()
    af "logistic" logisticFunc logisticFuncPtr
    af "tac_max" maxFunc maxFuncPtr
    af "tac_min" minFunc minFuncPtr
    af "writeSample" writeSampleFunc writeSampleFuncPtr
    af "llvm.exp.f64" expFunc expFuncPtr
    af "llvm.sin.f64" sinFunc sinFuncPtr
    af "llvm.tan.f64" tanFunc tanFuncPtr
    generateFunction genFunc
  
  generate (fromIntegral n)
  
  readIORef listRef

    
  

main = do
  initializeNativeTarget
  r <- runProgram 12 TAC.prog1
  print r