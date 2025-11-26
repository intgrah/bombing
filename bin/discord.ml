open Base
open Ppx_yojson_conv_lib.Yojson_conv

type token_request = { code : string } [@@deriving yojson]
type token_response = { access_token : string } [@@deriving yojson]

let vite_discord_client_id = Sys.getenv_exn "VITE_DISCORD_CLIENT_ID"
let discord_client_secret = Sys.getenv_exn "DISCORD_CLIENT_SECRET"

(* POST /api/token *)
let token_handler request =
  let open Lwt.Let_syntax in
  let uri = Uri.of_string "https://discord.com/api/oauth2/token" in
  let headers =
    Cohttp.Header.init_with "Content-Type" "application/x-www-form-urlencoded"
  in
  let%bind body = Dream.body request in
  let t_req = token_request_of_yojson @@ Yojson.Safe.from_string body in

  let query =
    Uri.encoded_of_query
      [
        ("client_id", [ vite_discord_client_id ]);
        ("client_secret", [ discord_client_secret ]);
        ("grant_type", [ "authorization_code" ]);
        ("code", [ t_req.code ]);
      ]
  in

  let%bind _, body_stream =
    Cohttp_lwt_unix.Client.post uri ~headers
      ~body:(Cohttp_lwt.Body.of_string query)
  in

  let%bind body_str = Cohttp_lwt.Body.to_string body_stream in

  let json_resp = Yojson.Safe.from_string body_str in
  let access_token =
    Yojson.Safe.Util.member "access_token" json_resp |> Yojson.Safe.to_string
  in

  Dream.json (yojson_of_token_response { access_token } |> Yojson.Safe.to_string)

type user = {
  id : string;
  username : string;
  avatar : string;
  (* discriminator : string; *)
  (* public_flags : int; *)
  (* flags : int; *)
  (* banner : string; *)
  (* accent_color : string option; *)
  (* global_name : string option; *)
  (* avatar_decoration_data : string option; *)
  (* collectibles : string option; *)
  (* banner_color : string option; *)
  (* clan : string option; *)
  (* primary_guild : string option; *)
  (* mfa_enabled : bool; *)
  (* locale : string; *)
  (* premium_type : int; *)
}
[@@deriving of_yojson]

let get_user (access_token : string) : user option Lwt.t =
  let uri = Uri.of_string "https://discord.com/api/v10/users/@me" in
  let headers =
    Cohttp.Header.add (Cohttp.Header.init ()) "authorization"
      ("Bearer " ^ access_token)
  in
  let%lwt resp, body = Cohttp_lwt_unix.Client.get ~headers uri in
  match Cohttp.Response.status resp with
  | `OK -> (
      let%lwt body_string = Cohttp_lwt.Body.to_string body in
      try
        Yojson.Safe.from_string body_string |> user_of_yojson |> Lwt.return_some
      with Yojson.Json_error _ -> Lwt.return_none)
  | _ -> Lwt.return_none
