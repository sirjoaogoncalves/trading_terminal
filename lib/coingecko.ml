(* lib/coingecko.ml *)
open Lwt.Syntax
open Cohttp_lwt_unix
open Types


type ticker = {
  price: float;
  volume_24h: float;
  price_change_24h: float;
  market_cap: float option;
  last_updated: string;
}

let base_url = "https://api.coingecko.com/api/v3"

let get_json endpoint =
  let url = Uri.of_string (base_url ^ endpoint) in
  let* (resp, body) = Client.get url in
  match Cohttp.Response.status resp with
  | `OK ->
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return_ok (Yojson.Safe.from_string body_str)
  | status ->
      let status_str = Cohttp.Code.string_of_status status in
      Lwt.return_error (NetworkError status_str)

let get_ticker symbol =
  let endpoint = Printf.sprintf 
    "/simple/price?ids=%s&vs_currencies=usd&include_market_cap=true&include_24hr_vol=true&include_24hr_change=true&include_last_updated_at=true"
    symbol in
  
  let* result = get_json endpoint in
  match result with
  | Ok json ->
      begin try
        let open Yojson.Safe.Util in
        let data = json |> member symbol |> to_assoc in
        let price = List.assoc "usd" data |> to_float in
        let volume = List.assoc "usd_24h_vol" data |> to_float in
        let change = List.assoc "usd_24h_change" data |> to_float in
        let market_cap = try Some (List.assoc "usd_market_cap" data |> to_float) with _ -> None in
        let updated = List.assoc "last_updated_at" data |> to_int |> string_of_int in
        
        Lwt.return_ok {
          price;
          volume_24h = volume;
          price_change_24h = change;
          market_cap;
          last_updated = updated;
        }
      with e ->
        Lwt.return_error (ParseError (Printexc.to_string e))
      end
  | Error e -> Lwt.return_error e
