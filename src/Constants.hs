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
numSimpleObstacles = 6
numDamageObstacles = 2
numSwirlObstacles = 2
numMineObstacles = 5

--size :: Float
--size = 400

sizeX,sizeY :: Float
sizeX = 700
sizeY = 400

tileSize :: Float
tileSize = 64 -- Tamaño de cada cuadrado del fondo en píxeles