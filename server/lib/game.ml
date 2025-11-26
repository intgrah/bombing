open Base
open Types
open Ppx_yojson_conv_lib.Yojson_conv

module Perspective = struct
  (* South (self), East, North (teammate), West *)
  type ('s, 'e, 'n, 'w) t = { s : 's; e : 'e; n : 'n; w : 'w }
  [@@deriving show, yojson_of]

  let players (from : Player.t) : (Player.t, Player.t, Player.t, Player.t) t =
    {
      s = Fn.apply_n_times ~n:0 Player.next from;
      e = Fn.apply_n_times ~n:1 Player.next from;
      n = Fn.apply_n_times ~n:2 Player.next from;
      w = Fn.apply_n_times ~n:3 Player.next from;
    }

  let map (f : 'a -> 'b) (x : ('a, 'a, 'a, 'a) t) : ('b, 'b, 'b, 'b) t =
    { s = f x.s; e = f x.e; n = f x.n; w = f x.w }

  let seq (f : ('sa -> 'sb, 'ea -> 'eb, 'na -> 'nb, 'wa -> 'wb) t)
      (x : ('sa, 'ea, 'na, 'wa) t) : ('sb, 'eb, 'nb, 'wb) t =
    { s = f.s x.s; e = f.e x.e; n = f.n x.n; w = f.w x.w }
end

type 'h t =
  | Playing of {
      hands : 'h;
      level : level;
      finished : finished;
      turn : Player.t;
      current : (Player.t * Score.t) option;
    }
  | Trading of {
      hands : 'h;
      level : level;
      position : Player.position Player.store;
      remaining : int Player.store;
    }
  | Winner of { team : Player.team; level : level }

and server_t = Card.t list Player.store t
and client_t = (Card.t list, int, int, int) Perspective.t t
and level = { ac : Rank.t; bd : Rank.t; who : Player.team }

and finished =
  | No_one
  | Only_big_master of Player.t
  | Both_masters of Player.t * Player.t
[@@deriving show, yojson_of]

let project_hand (view : Player.t) (hands : Card.t list Player.store) :
    (Card.t list, int, int, int) Perspective.t =
  Perspective.players view
  |> Perspective.map (Fn.flip Player.get hands)
  |> Perspective.seq
       { s = Fn.id; e = List.length; n = List.length; w = List.length }

let project (view : Player.t) : server_t -> client_t = function
  | Playing p -> Playing { p with hands = project_hand view p.hands }
  | Trading t -> Trading { t with hands = project_hand view t.hands }
  | Winner w -> Winner w

let rank_of : level -> Rank.t = function
  | { ac = r; bd = _; who = AC } | { ac = _; bd = r; who = BD } -> r

type action =
  | Revolt
  | Trade of Card.t
  | Play of Card.t list (* Score.t *)
  | Pass
[@@deriving show, of_yojson]

let deal_hands () : Card.t list Player.store =
  let deck =
    Card.all @ Card.all
    |> List.map ~f:(fun c -> (Random.bits (), c))
    |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst)
    |> List.map ~f:snd |> List.sub ~len:27
  in
  { a = deck ~pos:0; b = deck ~pos:27; c = deck ~pos:54; d = deck ~pos:81 }

let new_game () : server_t =
  Playing
    {
      hands = deal_hands ();
      level = { ac = Two; bd = Two; who = AC };
      finished = No_one;
      turn = A;
      current = None;
    }

let transition (state : server_t) (player : Player.t) (msg : action) :
    (server_t * _ list, _) Result.t =
  let open Result.Let_syntax in
  let rec multiset_subtract (xs : Card.t list) =
    let rec sub y = function
      | [] -> Error `Not_enough_cards
      | h :: hs when Card.equal h y -> Ok hs
      | h :: hs -> Result.map ~f:(List.cons h) (sub y hs)
    in
    function
    | [] -> Ok xs
    | y :: ys -> sub y xs |> Result.bind ~f:(Fn.flip multiset_subtract ys)
  in

  match (state, msg) with
  | Trading { remaining; _ }, Trade _ when Player.get player remaining = 0 ->
      Error `Already_traded
  | Trading _, Revolt -> Error `Not_implemented (* TODO: implement *)
  | Trading { hands; level; position; remaining }, Trade card -> (
      let hand = Player.get player hands in
      let%bind () =
        let level = rank_of level in
        (* Ensure the correct quantity of cards are being traded,
           and ensure that slaves give their highest cards *)
        match (Player.get player position, Rankj.of_card card) with
        | Big_master, _ | Small_master, _ -> Ok ()
        | Small_slave, offered | Big_slave, offered ->
            if
              hand
              |> List.filter ~f:(Fn.non @@ Card.equal (R (level, Hearts)))
              |> List.map ~f:Rankj.of_card
              |> List.max_elt ~compare:(Rankj.compare_at level)
              |> Option.value_map ~default:true ~f:(fun high ->
                     Rankj.compare_at level high offered <= 0)
            then Ok ()
            else Error `Must_give_highest_cards
      in

      let receiver : Player.t =
        match Player.get player position with
        | Big_master -> Player.who_is Big_slave position
        | Big_slave -> Player.who_is Big_master position
        | Small_master -> Player.who_is Small_slave position
        | Small_slave -> Player.who_is Small_master position
      in
      let%bind hand' = multiset_subtract hand [ card ] in
      let hands' =
        hands |> Player.set player hand'
        |> Player.set receiver (card :: Player.get receiver hands)
      in
      match
        Player.set player (Int.( - ) (Player.get player remaining) 1) remaining
      with
      | { a = 0; b = 0; c = 0; d = 0 } ->
          Ok
            ( Playing
                {
                  hands = hands';
                  level;
                  finished = No_one;
                  turn = Player.who_is Big_slave position;
                  current = None;
                },
              [ `Trade (player, receiver, card) ] )
      | remaining' ->
          Ok
            ( Trading { hands = hands'; level; position; remaining = remaining' },
              [ `Trade (player, receiver, card) ] ))
  | Playing { turn; _ }, (Play _ | Pass) when not (Player.equal player turn) ->
      Error `Not_your_turn
  | Playing { hands; level; finished; turn; current }, Pass ->
      let%bind leader, _ = Result.of_option current ~error:`Cannot_pass in
      let rec next_state (turn : Player.t) : server_t =
        let turn' = Player.next turn in
        match Player.(get turn' hands |> List.is_empty, equal turn' leader) with
        | true, true ->
            let turn'' = Player.teammate turn' in
            let turn''' =
              if Player.get turn'' hands |> List.is_empty then Player.next turn'
              else turn''
            in
            Playing { hands; level; finished; turn = turn'''; current = None }
        | true, false -> next_state turn'
        | false, true ->
            Playing { hands; level; finished; turn = turn'; current = None }
        | false, false ->
            Playing { hands; level; finished; turn = turn'; current }
      in

      Ok (next_state turn, [ `Passed player ])
  | Playing { hands; level; finished; turn; current }, Play (* score', *) cards'
    -> (
      let%bind score' =
        match Score.infer (rank_of level) (* score', *) cards' with
        | [] -> Error `Wrong_score
        | sc :: _ -> Ok sc
      in
      let%bind () =
        match current with
        | Some (_, score) when not (Score.lt_at (rank_of level) score score') ->
            Error `Doesn't_beat
        | _ -> Ok ()
      in
      let%bind hand' = multiset_subtract (Player.get player hands) cards' in
      let hands' = Player.set player hand' hands in

      let rec find_next (player : Player.t) : Player.t =
        let next_player = Player.next player in
        if Player.get next_player hands |> Fn.non List.is_empty then next_player
        else find_next next_player
      in
      let continue (finished' : finished) =
        Ok
          ( Playing
              {
                hands = hands';
                level;
                finished = finished';
                turn = find_next turn;
                current = Some (player, score');
              },
            [ `Played (player, score') ] )
      in
      match (hand', finished) with
      | _ :: _, _ -> continue finished
      | [], No_one -> continue (Only_big_master player)
      | [], Only_big_master bm -> (
          (* Attempt to short circuit *)
          match Player.(level, team bm, team player) with
          | { ac = Ace; bd = _; who = AC }, AC, AC ->
              Ok
                ( Winner { team = Player.AC; level },
                  [ `Played (player, score'); `Won (Player.AC, level) ] )
          | { ac = _; bd = Ace; who = BD }, BD, BD ->
              Ok
                ( Winner { team = Player.BD; level },
                  [ `Played (player, score'); `Won (Player.BD, level) ] )
          | _ -> continue (Both_masters (bm, player)))
      | [], Both_masters (bm, sm) ->
          let { ac; bd; who = _ } = level in
          let level' : level =
            Rank.(
              match Player.(team bm, team sm, team player) with
              | AC, AC, BD -> { ac = ac |> succ |> succ |> succ; bd; who = AC }
              | AC, BD, AC -> { ac = ac |> succ |> succ; bd; who = AC }
              | AC, BD, BD -> { ac = ac |> succ; bd; who = AC }
              | BD, BD, AC -> { ac; bd = bd |> succ |> succ |> succ; who = BD }
              | BD, AC, BD -> { ac; bd = bd |> succ |> succ; who = BD }
              | BD, AC, AC -> { ac; bd = bd |> succ; who = BD }
              | _ -> failwith "Invalid teams")
          in
          let position' =
            Player.(
              init Big_slave |> set bm Big_master |> set sm Small_master
              |> set player Small_slave)
          in
          let remaining' = Player.(init 2 |> set sm 1 |> set player 1) in
          Ok
            ( Trading
                {
                  hands = deal_hands ();
                  level = level';
                  position = position';
                  remaining = remaining';
                },
              [ `Played (player, score'); `New_round position' ] ))
  | Winner _, _
  | Trading _, Play _
  | Trading _, Pass
  | Playing _, Revolt
  | Playing _, Trade _ ->
      Error `Wrong_phase
