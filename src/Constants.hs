module Constants where

import Entities
import Config

import System.IO.Unsafe (unsafePerformIO)

globalConfig :: Config
globalConfig = unsafePerformIO $ readConfig "Configuration/config.txt"

numTanks :: Int
numTanks = 5

tankSpeed :: Float
tankSpeed = 25

tankWidth :: Float
tankWidth = 40
tankLength :: Float
tankLength = 100


projectileSpeed :: Float
projectileSpeed = 50
projectileDamage :: Int
projectileDamage = 20

numSimpleObstacles, numDamageObstacles, numSwirlObstacles, numMineObstacles :: Int
numSimpleObstacles = 3
numDamageObstacles = 2
numSwirlObstacles = 1
numMineObstacles = 4

tileSize :: Float
tileSize = 64 -- Tamaño de cada cuadrado del fondo en píxeles

getSizeX :: Float
getSizeX = fromIntegral $ (sizeX globalConfig)

getSizeY :: Float
getSizeY = fromIntegral $ (sizeY globalConfig)

getMaximumTime :: Float
getMaximumTime = maximunTime globalConfig

getNumTournaments :: Int
getNumTournaments = numTournaments globalConfig

getBotsList :: [String]
getBotsList = botsList globalConfig