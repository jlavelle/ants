   let t = constructors ./Types/Turn.dhall
in let L = t.L
in let R = t.R
in let U = t.U
in let N = t.N

in { original  = [R, L]
   , chaotic   = [R, L, R]
   , symmetric = [L, L, R, R]
   , highway   = [L, L, R, R, R, L, R, L, R, L, L, R]
   , fillSpace = [L, R, R, R, R, R, L, L, R]
   , triangle  = [R, R, L, L, L, R, L, L, L, R, R, R]
   }