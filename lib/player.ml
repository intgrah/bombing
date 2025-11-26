type t = A | B | C | D [@@deriving show, eq]
type team = AC | BD [@@deriving show]

let next : t -> t = function A -> B | B -> C | C -> D | D -> A
let teammate : t -> t = Fun.compose next next
let team : t -> team = function A | C -> AC | B | D -> BD

type 'a store = { a : 'a; b : 'a; c : 'a; d : 'a } [@@deriving show]

let get (p : t) ({ a; b; c; d } : 'a store) : 'a =
  match p with A -> a | B -> b | C -> c | D -> d

let init (x : 'a) : 'a store = { a = x; b = x; c = x; d = x }

let set (player : t) (x : 'a) (store : 'a store) : 'a store =
  match player with
  | A -> { store with a = x }
  | B -> { store with b = x }
  | C -> { store with c = x }
  | D -> { store with d = x }

type position = Big_master | Small_master | Small_slave | Big_slave
[@@deriving show, eq]

let mapi ~(f : t -> 'a -> 'b) ({ a; b; c; d } : 'a store) : 'b store =
  { a = f A a; b = f B b; c = f C c; d = f D d }

let to_list ({ a; b; c; d } : 'a store) : 'a list = [ a; b; c; d ]

let who_is (pos : position) ({ a; b; c; d } : position store) : t =
  if equal_position pos a then A
  else if equal_position pos b then B
  else if equal_position pos c then C
  else if equal_position pos d then D
  else failwith "Not found"
