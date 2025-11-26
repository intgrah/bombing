open Types

type t =
  | Playing of {
      hands : Card.t list Player.store;
      level : level;
      finished : finished;
      turn : Player.t;
      current : (Player.t * Score.t) option;
    }
  | Trading of {
      hands : Card.t list Player.store;
      level : level;
      position : Player.position Player.store;
      remaining : int Player.store;
    }
  | Winner of { team : Player.team; level : level }

and level = { ac : Rank.t; bd : Rank.t; who : Player.team }

and finished =
  | No_one
  | Only_big_master of Player.t
  | Both_masters of Player.t * Player.t
[@@deriving show]

let rank_of : level -> Rank.t = function
  | { ac = r; bd = _; who = AC } | { ac = _; bd = r; who = BD } -> r

type action =
  | Revolt
  | Trade of Card.t
  | Play of Card.t list (* Score.t *)
  | Pass
[@@deriving show]

let deal_hands () : Card.t list Player.store =
  let deck =
    Card.all @ Card.all
    |> List.map (fun c -> (Random.bits (), c))
    |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
    |> List.map snd
  in
  let sub pos len = List.filteri (fun i _ -> i >= pos && i < pos + len) deck in
  { a = sub 0 27; b = sub 27 27; c = sub 54 27; d = sub 81 27 }

let new_game () : t =
  Playing
    {
      hands = deal_hands ();
      level = { ac = Two; bd = Two; who = AC };
      finished = No_one;
      turn = A;
      current = None;
    }

let transition (state : t) (player : Player.t) (msg : action) :
    (t * _ list, _) result =
  let ( let* ) = Result.bind in
  let rec multiset_subtract (xs : Card.t list) =
    let rec sub y = function
      | [] -> Error `Not_enough_cards
      | h :: hs when Card.equal h y -> Ok hs
      | h :: hs -> Result.map (fun tl -> h :: tl) (sub y hs)
    in
    function
    | [] -> Ok xs
    | y :: ys -> Result.bind (sub y xs) (Fun.flip multiset_subtract ys)
  in

  match (state, msg) with
  | Trading { remaining; _ }, Trade _ when Player.get player remaining = 0 ->
      Error `Already_traded
  | Trading _, Revolt -> Error `Not_implemented (* TODO: implement *)
  | Trading { hands; level; position; remaining }, Trade card -> (
      let hand = Player.get player hands in
      let* () =
        let level = rank_of level in
        (* Ensure the correct quantity of cards are being traded,
           and ensure that slaves give their highest cards *)
        match (Player.get player position, Rankj.of_card card) with
        | Big_master, _ | Small_master, _ -> Ok ()
        | Small_slave, offered | Big_slave, offered ->
            let dominated =
              hand
              |> List.filter (Fun.negate @@ Card.equal (R (level, Hearts)))
              |> List.map Rankj.of_card
              |> List.fold_left
                   (fun acc rj ->
                     match acc with
                     | None -> Some rj
                     | Some high ->
                         if Rankj.compare_at level rj high > 0 then Some rj
                         else acc)
                   None
              |> Option.fold ~none:true ~some:(fun high ->
                  Rankj.compare_at level high offered <= 0)
            in
            if dominated then Ok () else Error `Must_give_highest_cards
      in

      let receiver : Player.t =
        match Player.get player position with
        | Big_master -> Player.who_is Big_slave position
        | Big_slave -> Player.who_is Big_master position
        | Small_master -> Player.who_is Small_slave position
        | Small_slave -> Player.who_is Small_master position
      in
      let* hand' = multiset_subtract hand [ card ] in
      let hands' =
        hands |> Player.set player hand'
        |> Player.set receiver (card :: Player.get receiver hands)
      in
      match Player.set player (Player.get player remaining - 1) remaining with
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
      let* leader, _ = Option.to_result ~none:`Cannot_pass current in
      let rec next_state (turn : Player.t) : t =
        let turn' = Player.next turn in
        match (Player.get turn' hands = [], Player.equal turn' leader) with
        | true, true ->
            let turn'' = Player.teammate turn' in
            let turn''' =
              if Player.get turn'' hands = [] then Player.next turn' else turn''
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
      let* score' =
        match Score.infer (rank_of level) (* score', *) cards' with
        | [] -> Error `Wrong_score
        | sc :: _ -> Ok sc
      in
      let* () =
        match current with
        | Some (_, score) when not (Score.lt_at (rank_of level) score score') ->
            Error `Doesn't_beat
        | _ -> Ok ()
      in
      let* hand' = multiset_subtract (Player.get player hands) cards' in
      let hands' = Player.set player hand' hands in

      let rec find_next (player : Player.t) : Player.t =
        let next_player = Player.next player in
        if Player.get next_player hands <> [] then next_player
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
