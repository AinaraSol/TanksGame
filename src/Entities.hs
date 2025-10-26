module Entities where

import Types
import Graphics.Gloss.Data.Picture (Picture)

type Bot = GameState -> Tank -> [Action]

-- Action: Acciones que puede realizar un tanque
data Action = Move Vector | Shoot Position  |  Rotate Angle  | Stay
    deriving (Show, Eq)

-- Tipos de Datos necesarios --

---- Tipo Tank --
-- Tiene la propiedad tankPosition que nos indica la posición del tanque en el tablero,
-- la propiedad turret es del tipo Turret que nos da información sobre la torreta
-- orientation nos indica hacia donde apunta el tanque, health es la propiedad que indica
-- la vida restante del tanque.


data Tank = 
    Tanque {
        idTank::Int, 
        --tankPosition::Position, 
        turret::Turret, 
        --orientation::Angle, 
        health:: Maybe Int,
        --velocity::Velocity, 
        memory::Memory,
        tankBaseObject :: BaseObject, -- Composicion
        shootCooldown :: Float, --Tiempo para enfriar el cañon
        tankPicture :: Picture
    }
    deriving (Show, Eq)

---- Tipo Turret --
-- La propiedad turretOrientation indica hacia donde apunta la torreta. Esta orientacion puede
-- ser diferente a la del tanque.

data Turret = 
    Turret {
        turretOrientation::Angle,
        turretPicture :: Picture
    }
    deriving (Show, Eq)

---- Tipo Proyectile --
-- La propiedad origin indica la posición desde donde se dispara el proyectil, la propiedad 
-- proyectilePosition es la posicion en la que se encuentra el proyectil, la propiedad angle
-- es el angulo con el que fue disparado el proyectil.

data Proyectile = 
    Proyectile {
        origin::Position, 
        --proyectilePosition::Position,
        --angle::Angle,
        damage :: Int,
        proyectileBaseObject :: BaseObject -- Composicion
    }
    deriving (Show,Eq)


-- GameState: Estado completo del juego
data GameState = GameState {
    tanks :: [Tank], --tanques vivos en el momento
    proyectiles :: [Proyectile], -- proyectiles activos en pantalla
    explosions :: [Explosion], -- explosiones en el tablero
    worldSize :: Size, -- cuadrícula del juego no sabemos si declararla como variable global debido a que será constante
    gameTime :: Float,
    winner :: Maybe (Int, Float), -- <-- Guardamos al ganador del juego
    background :: Picture, -- Imagen de fondo del juego
    projectilePicture :: Picture, -- Imagen del proyectil
    explosionPictures :: [Picture] -- Lista de imágenes para la animación de explosiones
} deriving (Show)

data Explosion = Explosion {
    explosionPosition :: Position,
    explosionTime :: Float
} deriving (Show)

data MemoryValue
    = MemInt (Maybe Int)
    | MemPoint (Maybe Point)
    | MemString (Maybe String)
    deriving (Show, Eq)

type Memory = [(String, MemoryValue)]

-- Crear tipo genérico base: Diseña un tipo de datos genérico que abstraiga las propiedades 
-- comunes de todos los objetos del juego (posición, velocidad, tamaño, etc.).

data BaseObject = BaseObject { 
    position :: Position,
    velocity :: Velocity, 
    orientation :: Angle
} deriving (Show, Eq)
