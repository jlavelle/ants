   let Direction = ./Direction.dhall
in let Turn = ./Turn.dhall

in { direction : Direction
   , rule      : List Turn
   , start     : { x : Integer, y : Integer }
   }
