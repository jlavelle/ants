   let Direction = ./Direction.dhall

in let Turn = ./Turn.dhall

in let Ant = { direction : Direction
             , rule      : List Turn
             , start     : { x : Integer, y : Integer }
             }

in let Color = { r : Natural
               , g : Natural
               , b : Natural
               , a : Natural
               }

in { ants   : List Ant
   , colors : List Color
   }