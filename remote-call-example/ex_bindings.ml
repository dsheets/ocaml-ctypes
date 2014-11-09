open Ctypes

module Bindings(F : Cstubs.FOREIGN) =
struct
  let plus3 = F.foreign "ex_plus3" (int @-> int @-> int @-> returning int)
  let sqrt = F.foreign "ex_sqrt" (double @-> returning double)
end
