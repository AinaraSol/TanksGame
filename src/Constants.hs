module Constants where

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

--size :: Float
--size = 400

sizeX, sizeY :: Float
sizeX = 650
sizeY = 340

tileSize :: Float
tileSize = 64 -- Tamaño de cada cuadrado del fondo en píxeles