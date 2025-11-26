open Base
open Guandan.Player

let () =
  let store : int store = { a = 1; b = 2; c = 3; d = 4 } in
  assert (store |> get A = 1);
  assert (store |> set A 5 |> get A = 5)
 