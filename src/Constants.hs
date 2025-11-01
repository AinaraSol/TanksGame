module Constants where

numTanks :: Int
numTanks = 8

tankSpeed :: Float
tankSpeed = 25

tankWidth :: Float
tankWidth = 40
tankLength :: Float
tankLength = 60


projectileSpeed :: Float
projectileSpeed = 50
projectileDamage :: Int
projectileDamage = 20

numSimpleObstacles, numDamageObstacles, numSwirlObstacles, numMineObstacles :: Int
numSimpleObstacles = 2
numDamageObstacles = 2
numSwirlObstacles = 1
numMineObstacles = 2

size :: Float
size = 400

tileSize :: Float
tileSize = 64 -- Tamaño de cada cuadrado del fondo en píxeles