(* lib/market_data.ml *)
let format_volume vol =
  if vol >= 1_000_000_000. then
    Printf.sprintf "%.2fB" (vol /. 1_000_000_000.)
  else if vol >= 1_000_000. then
    Printf.sprintf "%.2fM" (vol /. 1_000_000.)
  else if vol >= 1_000. then
    Printf.sprintf "%.2fK" (vol /. 1_000.)
  else
    Printf.sprintf "%.2f" vol

type state = {
  data: (string, Types.market_data) Hashtbl.t;
}

let default_pairs = [
  { Types.base = "bitcoin"; quote = "USD" };
  { Types.base = "ethereum"; quote = "USD" };
  { Types.base = "solana"; quote = "USD" };
  { Types.base = "binancecoin"; quote = "USD" };
]

let create () = {
  data = Hashtbl.create 10;
}

let log_market_data msg =
  Lwt_io.printl ("[MARKET] " ^ msg)

let update_ticker t symbol =
  let open Lwt.Syntax in
  let rec try_apis attempts =
    if attempts <= 0 then
      Lwt.return_error "All APIs failed"
    else
      let* () = log_market_data (Printf.sprintf "Trying API source: %s" 
        (match !Crypto_api.current_source with
         | Binance -> "Binance"
         | Kucoin -> "KuCoin"
         | Coingecko -> "CoinGecko"
         | CryptoCompare -> "CryptoCompare")) in
      
      let* result = Crypto_api.get_ticker symbol in
      match result with
      | Ok ticker -> 
          let* () = log_market_data (Printf.sprintf "Got price: %.2f" ticker.price) in
          Hashtbl.replace t.data symbol ticker;
          Lwt.return_ok ticker
      | Error msg ->
          let* () = log_market_data (Printf.sprintf "API failed: %s" msg) in
          Crypto_api.switch_source ();
          let* () = Lwt_unix.sleep 1.0 in (* Wait a bit before trying next API *)
          try_apis (attempts - 1)
  in
  try_apis 4 (* Try all 4 APIs before giving up *)

let get_ticker t symbol =
  Hashtbl.find_opt t.data symbol
