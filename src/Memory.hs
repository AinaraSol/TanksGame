module Memory where

import Types
import Entities

-- | Lectura genérica de memoria
readMemory :: String -> Memory -> Maybe MemoryValue
readMemory = lookup 

-- Funciones auxiliares para lectura por tipo
readMemoryInt :: String -> Memory -> Maybe (Maybe Int)
readMemoryInt key mem = case readMemory key mem of
    Just (MemInt v) -> Just v
    _               -> Nothing

readMemoryPoint :: String -> Memory -> Maybe (Maybe Point)
readMemoryPoint key mem = case readMemory key mem of
    Just (MemPoint v) -> Just v
    _                 -> Nothing

readMemoryString :: String -> Memory -> Maybe (Maybe String)
readMemoryString key mem = case readMemory key mem of
    Just (MemString v) -> Just v
    _                  -> Nothing

-- | Escritura o actualización de un valor en memoria
writeMemory :: String -> MemoryValue -> Memory -> Memory
writeMemory key val mem = (key, val) : filter (\(k,_) -> k /= key) mem

-- Funciones auxiliares para escritura por tipo
writeMemoryInt :: String -> Int -> Memory -> Memory
writeMemoryInt key val = writeMemory key (MemInt (Just val)) 

writeMemoryPoint :: String -> Point -> Memory -> Memory
writeMemoryPoint key val = writeMemory key (MemPoint (Just val)) 

writeMemoryString :: String -> String -> Memory -> Memory
writeMemoryString key val = writeMemory key (MemString (Just val))

-- Funciones auxiliares para actualizar valores existentes
updateMemoryInt :: String -> (Int -> Int) -> Memory -> Memory
updateMemoryInt key f mem =
    case readMemoryInt key mem of
        Just (Just val) -> writeMemoryInt key (f val) mem
        _               -> mem

updateMemoryPoint :: String -> (Point -> Point) -> Memory -> Memory
updateMemoryPoint key f mem =
    case readMemoryPoint key mem of
        Just (Just val) -> writeMemoryPoint key (f val) mem
        _               -> mem

updateMemoryString :: String -> (String -> String) -> Memory -> Memory
updateMemoryString key f mem =
    case readMemoryString key mem of
        Just (Just val) -> writeMemoryString key (f val) mem
        _               -> mem
