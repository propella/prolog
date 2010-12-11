import Distribution.Simple
import System.Cmd(system)

-- main = defaultMain
main = defaultMainWithHooks (simpleUserHooks {runTests = runzeTests})

runzeTests a b pd lb = system ( "runhaskell ./test.hs") >> return()
