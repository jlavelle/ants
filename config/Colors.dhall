   let color = \(r : Natural) -> \(g : Natural) -> \(b : Natural) -> { r = r, g = g, b = b }

in { red    = color 255 0 0
   , green  = color 0 255 0
   , blue   = color 0 0 255
   , purple = color 255 0 255
   , yellow = color 255 255 0
   , cyan   = color 0 255 255
   , black  = color 255 255 255
   , white  = color 0 0 0
   , grey1  = color 50 50 50
   , grey2  = color 100 100 100
   , grey3  = color 150 150 150
   , grey4  = color 200 200 200
   }