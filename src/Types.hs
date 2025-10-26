module Types where

--       - Point. Un punto 2D en el espacio.
type Point = (Float, Float)

--       - Vector. Vector siempre se considera que empieza en (0,0)
type Vector = Point

--       - Angle. Un angulo con decimales
type Angle = Float

--       - Distance. Un valor de distancia con decimales.
type Distance = Float

--      - Position. Representa la posición de objeto en un mundo 2D.
type Position = Point

--      - Size. Un rectángulo que empieza en el punto (0,0) y tiene un ancho de Size.
type Size = (Float, Float)

--      - Bot. Creacion de un bot que recibe el estado del juego, un tank y devuelve una serie de acciones.

type Velocity = Vector
