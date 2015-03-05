module Yabi.Parser
    ( programParser
    ) where

import Control.Monad
import Data.Functor
import Text.Parsec

import Yabi.Types

nothings :: Parsec String () Char
nothings = noneOf "><+-.,[]"

programParser :: Parsec String () [Inst]
programParser = do
    is <- instsParser
    eof
    return is

instsParser :: Parsec String () [Inst]
instsParser = skipMany nothings >> instParser `sepEndBy` many nothings

instParser :: Parsec String () Inst
instParser = msum
    [ nextParser
    , prevParser
    , incrParser
    , decrParser
    , putCParser
    , getCParser
    , loopParser
    ]

nextParser :: Parsec String () Inst
nextParser = char '>' >> return Next

prevParser :: Parsec String () Inst
prevParser = char '<' >> return Prev

incrParser :: Parsec String () Inst
incrParser = char '+' >> return Incr

decrParser :: Parsec String () Inst
decrParser = char '-' >> return Decr

putCParser :: Parsec String () Inst
putCParser = char '.' >> return PutC

getCParser :: Parsec String () Inst
getCParser = char ',' >> return GetC

loopParser :: Parsec String () Inst
loopParser = Loop <$> between (char '[') (char ']') instsParser
