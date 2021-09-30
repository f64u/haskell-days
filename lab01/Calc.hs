-- The calculator program. Takes reads the arguments and then
-- calls the solveRPN function you defined in AbstractInteger.hs
-- and prints the result.
--
-- You should not need to modify this file.
--

import           AbstractInteger
import qualified System.Environment

main :: IO ()
main = do
    args <- System.Environment.getArgs
    print . fromAbstract . evaluateRPN  $ args
