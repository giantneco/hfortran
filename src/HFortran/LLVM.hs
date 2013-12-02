-- borrowed from https://gist.github.com/robstewart57/7194104

import Control.Monad
import Data.Word
import LLVM.Core
import LLVM.Util.File
 
-- -- prints out "hello world"
-- bldGreet :: CodeGenModule (Function (IO ()))
-- bldGreet = do
--     puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
--     func <- withStringNul "Hello, World!" $ \greetz ->
--       createFunction ExternalLinkage $ do
--         tmp <- getElementPtr greetz (0::Word32, (0::Word32, ()))
--         void $ call puts tmp
--         ret ()
--     return func
 
-- main :: IO ()
-- main = writeCodeGenModule "hello.bc" (_main =<< bldGreet)
--   where
--     _main :: (Function (IO ())) -> CodeGenModule (Function (IO ()))
--     _main func = createNamedFunction ExternalLinkage "main" $ do
--       call func
--       ret ()
