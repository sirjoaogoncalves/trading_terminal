(* lib/crypto_api.ml *)
open Lwt.Syntax
open Cohttp_lwt_unix
open Types

type api_source = 
  | Binance
  | Kucoin
  | Coingecko
  | CryptoCompare

let current_source = ref Binance

let switch_source () =
  current_source := match !current_source with
  | Binance -> Kucoin
  | Kucoin -> Coingecko
  | Coingecko -> CryptoCompare
  | CryptoCompare -> Binance

let normalize_symbol symbol =
  String.uppercase_ascii @@ 
  match symbol with
  | "bitcoin" -> "BTC"
  | "ethereum" -> "ETH"
  | "solana" -> "SOL"
  | "binancecoin" -> "BNB"
  | s -> s

let get_binance_ticker symbol =
  let symbol = normalize_symbol symbol in
  let url = Uri.of_string (Printf.sprintf 
    "https://api.binance.com/api/v3/ticker/24hr?symbol=%sUSDT" 
    symbol) in
  
  let* (resp, body) = Client.get url in
  match Cohttp.Response.status resp with
  | `OK ->
      let* body_str = Cohttp_lwt.Body.to_string body in
      begin try
        let json = Yojson.Safe.from_string body_str in
        let open Yojson.Safe.Util in
        {
          price = json |> member "lastPrice" |> to_string |> float_of_string;
          volume_24h = json |> member "volume" |> to_string |> float_of_string;
          change_24h = json |> member "priceChangePercent" |> to_string |> float_of_string;
          market_cap = None; (* Binance doesn't provide market cap *)
          last_updated = json |> member "closeTime" |> to_string;
        } |> fun ticker -> Lwt.return_ok ticker
      with e ->
        Lwt.return_error (Printf.sprintf "Parse error: %s" (Printexc.to_string e))
      end
  | _ -> Lwt.return_error "Failed to fetch from Binance"

let get_kucoin_ticker symbol =
  let symbol = normalize_symbol symbol in
  let url = Uri.of_string (Printf.sprintf 
    "https://api.kucoin.com/api/v1/market/stats?symbol=%s-USDT" 
    symbol) in
  
  let* (resp, body) = Client.get url in
  match Cohttp.Response.status resp with
  | `OK ->
      let* body_str = Cohttp_lwt.Body.to_string body in
      begin try
        let json = Yojson.Safe.from_string body_str in
        let open Yojson.Safe.Util in
        let data = json |> member "data" in
        {
          price = data |> member "last" |> to_string |> float_of_string;
          volume_24h = data |> member "vol" |> to_string |> float_of_string;
          change_24h = data |> member "changeRate" |> to_string |> float_of_string |> ( *. ) 100.0;
          market_cap = None;
          last_updated = string_of_float (Unix.time ());
        } |> fun ticker -> Lwt.return_ok ticker
      with e ->
        Lwt.return_error (Printf.sprintf "Parse error: %s" (Printexc.to_string e))
      end
  | _ -> Lwt.return_error "Failed to fetch from KuCoin"

let get_cryptocompare_ticker symbol =
  let symbol = normalize_symbol symbol in
  let url = Uri.of_string (Printf.sprintf 
    "https://min-api.cryptocompare.com/data/price?fsym=%s&tsyms=USD" 
    symbol) in
  
  let* (resp, body) = Client.get url in
  match Cohttp.Response.status resp with
  | `OK ->
      let* body_str = Cohttp_lwt.Body.to_string body in
      begin try
        let json = Yojson.Safe.from_string body_str in
        let open Yojson.Safe.Util in
        {
          price = json |> member "USD" |> to_float;
          volume_24h = 0.0; (* Basic endpoint doesn't include volume *)
          change_24h = 0.0;
          market_cap = None;
          last_updated = string_of_float (Unix.time ());
        } |> fun ticker -> Lwt.return_ok ticker
      with e ->
        Lwt.return_error (Printf.sprintf "Parse error: %s" (Printexc.to_string e))
      end
  | _ -> Lwt.return_error "Failed to fetch from CryptoCompare"

let get_ticker symbol =
  let* () = Lwt_unix.sleep 0.1 in (* Small delay to prevent hammering *)
  match !current_source with
  | Binance -> get_binance_ticker symbol
  | Kucoin -> get_kucoin_ticker symbol
  | CryptoCompare -> get_cryptocompare_ticker symbol
  | Coingecko -> 
      (* Fallback to original CoinGecko implementation but with very conservative rate limiting *)
      let url = Uri.of_string (Printf.sprintf 
        "https://api.coingecko.com/api/v3/simple/price?ids=%s&vs_currencies=usd&include_24hr_vol=true&include_24hr_change=true"
        (String.lowercase_ascii symbol)) in
      
      let* (resp, body) = Client.get url in
      match Cohttp.Response.status resp with
      | `OK ->
          let* body_str = Cohttp_lwt.Body.to_string body in
          begin try
            let json = Yojson.Safe.from_string body_str in
            let open Yojson.Safe.Util in
            let data = json |> member (String.lowercase_ascii symbol) |> to_assoc in
            {
              price = List.assoc "usd" data |> to_float;
              volume_24h = List.assoc "usd_24h_vol" data |> to_float;
              change_24h = List.assoc "usd_24h_change" data |> to_float;
              market_cap = None;
              last_updated = string_of_float (Unix.time ());
            } |> fun ticker -> Lwt.return_ok ticker
          with e ->
            Lwt.return_error (Printf.sprintf "Parse error: %s" (Printexc.to_string e))
          end
      | _ -> Lwt.return_error "Failed to fetch from CoinGecko"
