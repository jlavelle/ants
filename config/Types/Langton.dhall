let Direction = < North : {}
                | South : {}
                | East  : {}
                | West  : {}
                >

in let Turn = < L : {}
              | R : {}
              | N : {}
              | U : {}
              >

in let Ant = { direction : Direction
             , rule      : List Turn
             , start     : { x : Natural, y : Natural }
             }

in let Color = { r : Natural
               , g : Natural
               , b : Natural
               , a : Natural
               }

in { ants   : List Ant
   , colors : List Color
   }