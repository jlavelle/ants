   let Direction = ./Direction.dhall
in let Turn = ./Turn.dhall
in let Color = ./Color.dhall
in let Ant = ./Ant.dhall

in { ants       : List Ant
   , colors     : List Color
   , background : Color
   }