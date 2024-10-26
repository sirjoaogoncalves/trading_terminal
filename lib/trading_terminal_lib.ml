(* lib/trading_terminal_lib.ml *)
open Types
open Notty
open Notty.Infix

module Terminal = struct
  (* Remove this duplicate type definition *)
  (* type chart_state = {
    timeframe: chart_timeframe;
    start_index: int;
    data: market_data list;
  } *)

  let term = Notty_unix.Term.create ()

  let string_of_timeframe = function
    | OneHour -> "1H"
    | FourHour -> "4H"
    | OneDay -> "1D"
    | OneWeek -> "1W"

  let render_ticker ~width pair data =
    let height = 3 in
    let content = match data with
      | Some d ->
          let price_str = Printf.sprintf "%.2f" d.price in
          let volume_str = 
            if d.volume_24h >= 1_000_000_000. then
              Printf.sprintf "%.2fB" (d.volume_24h /. 1_000_000_000.)
            else if d.volume_24h >= 1_000_000. then
              Printf.sprintf "%.2fM" (d.volume_24h /. 1_000_000.)
            else if d.volume_24h >= 1_000. then
              Printf.sprintf "%.2fK" (d.volume_24h /. 1_000.)
            else
              Printf.sprintf "%.2f" d.volume_24h
          in
          let change_str = Printf.sprintf "%+.2f%%" d.change_24h in
          let change_style = if d.change_24h >= 0. then A.(fg green) else A.(fg red) in
          let mkt_cap_str = match d.market_cap with
            | Some cap -> Printf.sprintf " | MCap: %s" (
                if cap >= 1_000_000_000. then
                  Printf.sprintf "%.2fB" (cap /. 1_000_000_000.)
                else if cap >= 1_000_000. then
                  Printf.sprintf "%.2fM" (cap /. 1_000_000.)
                else if cap >= 1_000. then
                  Printf.sprintf "%.2fK" (cap /. 1_000.)
                else
                  Printf.sprintf "%.2f" cap
              )
            | None -> ""
          in
          
          I.string A.(fg white) "Price: " <|>
          I.string A.(fg green ++ st bold) price_str <|>
          I.string A.empty " | Vol: " <|>
          I.string A.(fg cyan) volume_str <|>
          I.string A.empty " | 24h: " <|>
          I.string change_style change_str <|>
          I.string A.empty mkt_cap_str
      | None ->
          I.string A.(fg yellow) "Loading Market Data..."
    in
    
    Ui.draw_box 
      ~title:(Printf.sprintf "%s/%s" (String.uppercase_ascii pair.base) pair.quote)
      ~width 
      ~height
      ~focused:false
      content

  let render_price_scale min_price max_price =   (* Removido o parâmetro ~height que não é usado *)
    let steps = 5 in
    let price_step = (max_price -. min_price) /. float_of_int steps in
    
    let scale = List.init (steps + 1) (fun i ->
      let price = max_price -. (float_of_int i *. price_step) in
      I.string A.(fg white) (Printf.sprintf "%8.2f" price)
    ) in
    
    List.fold_left (<->) I.empty scale

let render_chart ~width ~height ~focused ~chart_state current_data =
  let chart_style = if focused then A.(fg lightblue) else A.(fg white) in
  let up_style = A.(fg green) in
  let down_style = A.(fg red) in
  let grid_style = A.(fg blue) in
  let visible_width = max 10 (width - 12) in (* Ensure minimum width *)
  let chart_height = max 5 (height - 2) in (* Ensure minimum height *)
  
  match current_data with
  | None -> 
      Ui.draw_box 
        ~title:(Printf.sprintf "Price Chart (%s)" (string_of_timeframe chart_state.timeframe))
        ~width ~height ~focused
        (I.string A.(fg yellow) "Loading chart data...")
  
  | Some data ->
      (* Create synthetic price history - limit to visible width *)
      let num_points = min 60 visible_width in
      let points = 
        let base_price = data.price in
        let volatility = base_price *. 0.001 in
        List.init num_points (fun i ->
          let time = Unix.time () -. (float_of_int (num_points - i) *. 60.0) in
          let random_walk = 
            List.init i (fun _ -> 
              (Random.float 2.0 -. 1.0) *. volatility
            ) |> List.fold_left (+.) 0.0
          in
          {
            Types.price = base_price +. random_walk;
            volume_24h = data.volume_24h;
            change_24h = data.change_24h;
            market_cap = data.market_cap;
            last_updated = string_of_float time;
          }
        )
      in

      (* Find price range *)
      let prices = List.map (fun d -> d.price) points in
      let min_price = List.fold_left min max_float prices in
      let max_price = List.fold_left max min_float prices in
      let price_range = if min_price = max_price 
        then max_price *. 0.1 
        else max_price -. min_price in
      let min_price = min_price -. (price_range *. 0.05) in
      let max_price = max_price +. (price_range *. 0.05) in
      let price_range = max_price -. min_price in

      (* Initialize grid *)
      let grid = Array.make_matrix chart_height visible_width ' ' in
      
      (* Safe grid access *)
      let safe_set_grid y x c =
        if y >= 0 && y < chart_height && x >= 0 && x < visible_width then
          grid.(y).(x) <- c
      in
      
      (* Scale helpers *)
      let scale_y price =
        let normalized = (price -. min_price) /. price_range in
        let raw_y = int_of_float (float_of_int (chart_height - 1) *. (1.0 -. normalized)) in
        min (chart_height - 1) (max 0 raw_y)
      in

      (* Draw grid lines - less frequent to reduce clutter *)
      let h_lines = min 5 (chart_height / 2) in
      for i = 0 to h_lines do
        let y = (i * (chart_height - 1)) / h_lines in
        if y < chart_height then
          for x = 0 to visible_width - 1 do
            safe_set_grid y x '.'
          done
      done;

      (* Draw time markers *)
      let v_lines = min 5 (visible_width / 10) in
      for i = 0 to v_lines do
        let x = (i * (visible_width - 1)) / v_lines in
        for y = 0 to chart_height - 1 do
          safe_set_grid y x (if grid.(y).(x) = '.' then '+' else '|')
        done
      done;

      (* Plot prices *)
      List.iteri (fun i p ->
        if i > 0 then begin
          let prev_p = List.nth points (i - 1) in
          let x = (i * (visible_width - 1)) / (num_points - 1) in
          let prev_x = ((i - 1) * (visible_width - 1)) / (num_points - 1) in
          let y = scale_y p.price in
          let prev_y = scale_y prev_p.price in
          
          (* Draw line between points *)
          let dx = x - prev_x in
          let dy = y - prev_y in
          let steps = max 1 (max (abs dx) (abs dy)) in
          
          for step = 0 to steps do
            let t = float_of_int step /. float_of_int steps in
            let cx = prev_x + int_of_float (float_of_int dx *. t) in
            let cy = prev_y + int_of_float (float_of_int dy *. t) in
            let char = if prev_y > y then '/' else if prev_y < y then '\\' else '-' in
            safe_set_grid cy cx char
          done
        end else begin
          let x = 0 in
          let y = scale_y p.price in
          safe_set_grid y x 'o'
        end
      ) points;

      (* Convert grid to image *)
      let chart_img = 
        List.init chart_height (fun y ->
          I.hcat (List.init visible_width (fun x ->
            let c = grid.(y).(x) in
            match c with
            | '/' -> I.string up_style "/"
            | '\\' -> I.string down_style "\\"
            | '-' -> I.string chart_style "-"
            | 'o' -> I.string chart_style "o"
            | '.' | '+' | '|' -> I.string grid_style (String.make 1 c)
            | _ -> I.string A.empty " "
          ))
        ) |> List.fold_left (<->) I.empty
      in
      
      (* Price scale *)
      let price_scale = 
        let levels = min 6 chart_height in
        List.init (levels + 1) (fun i ->
          let price = max_price -. (float_of_int i *. price_range /. float_of_int levels) in
          I.string chart_style (Printf.sprintf "%10.2f" price)
        ) |> List.fold_left (<->) I.empty
      in
      
      (* Time labels *)
      let time_labels = match chart_state.timeframe with
        | OneHour -> "45m     30m     15m     5m      Now"
        | FourHour -> "3h      2h      1h      30m     Now"
        | OneDay -> "18h     12h     6h      3h      Now"
        | OneWeek -> "5d      4d      3d      2d      Now"
      in

      (* Stats *)
      let stats = Printf.sprintf "24h Change: %+.2f%% | Vol: %.2fM" 
        data.change_24h
        (data.volume_24h /. 1_000_000.) in
      
      let content =
        (chart_img <|> I.void 1 1 <|> price_scale) <->
        I.string chart_style time_labels <->
        I.string (if data.change_24h >= 0. then up_style else down_style) stats
      in
      
      Ui.draw_box 
        ~title:(Printf.sprintf "Price Chart (%s)" (string_of_timeframe chart_state.timeframe))
        ~width ~height ~focused
        content

  let render_market_overview ~width ~height ~focused current_state market_state =
  let header_style = A.(fg white ++ st bold) in
  let selected_style = A.(bg lightblue) in
  let normal_style = A.empty in
  
  (* Header *)
  let header = I.hcat [
    I.string header_style "Pair";
    I.void (width - 40) 1;
    I.string header_style "Price";
    I.void 4 1;
    I.string header_style "24h%";
  ] in
  
  (* Format helpers *)
  let format_price price =
    if price >= 1000.0 then
      Printf.sprintf "%.1f" price
    else if price >= 1.0 then
      Printf.sprintf "%.2f" price
    else
      Printf.sprintf "%.4f" price
  in
  
  let format_change change =
    Printf.sprintf "%+.1f%%" change
  in
  
  (* Render each pair *)
  let render_pair pair =
    let selected = pair = current_state.pair in
    match Hashtbl.find_opt market_state.Market_data.data pair.base with
    | Some data ->
        let style = if selected then selected_style else normal_style in
        let change_style = 
          if data.change_24h > 0.0 then A.(style ++ fg green)
          else A.(style ++ fg red) in
        
        I.hcat [
          I.string style (Printf.sprintf "%-8s/USDT" (String.uppercase_ascii pair.base));
          I.void (width - 40) 1;
          I.string style (Printf.sprintf "%12s" (format_price data.price));
          I.void 4 1;
          I.string change_style (Printf.sprintf "%8s" (format_change data.change_24h))
        ]
    | None ->
        I.string normal_style (Printf.sprintf "%-8s/USDT   Loading..." (String.uppercase_ascii pair.base))
  in
  
  let pairs = Market_data.default_pairs in
  
  (* Create content with scrolling support *)
  let content = 
    I.vcat (
      header ::
      I.void width 1 ::
      List.map render_pair pairs
    )
  in
  
  (* Add help text *)
  let help_text = I.string A.(fg lightblack) "Press Tab to focus | ↑↓ to navigate | Enter to select" in
  let final_content = 
    content <->
    I.void width 1 <->
    help_text
  in
  
  Ui.draw_box 
    ~title:"Market Overview" 
    ~width 
    ~height 
    ~focused 
    final_content

  let render state mkt_state =  
  let { size = { width; height }; pair; data; focus; chart_state; _ } = state in
  
  let status_msg = Printf.sprintf "Last Updated: %s" 
    (match data with 
     | Some d -> d.last_updated 
     | None -> "Never") in
  
  (* Calculate dimensions *)
  let ticker_height = 5 in
  let status_height = 1 in
  let keybinds_height = 1 in
  let main_height = height - ticker_height - status_height - keybinds_height - 1 in
  let chart_width = (width * 3) / 4 in
  let market_width = width - chart_width in
  
  (* Render components *)
  let ticker = render_ticker ~width pair data in
  let chart = render_chart 
              ~width:chart_width 
              ~height:main_height 
              ~focused:(focus = Chart)
              ~chart_state 
              data in
  let market = render_market_overview 
              ~width:market_width 
              ~height:main_height 
              ~focused:(focus = MarketOverview)
              state
              mkt_state in
  let status = Ui.render_status_bar ~width ~message:status_msg in
  let keybinds = Ui.render_keybinds ~width in
  
  (* Combine with separators *)
  let separator = I.char A.(fg white) '-' width 1 in
  
  ticker <->
  separator <->
  (chart <|> market) <->
  separator <->
  status <->
  keybinds

  let get_terminal_size () =
    let (w, h) = Notty_unix.Term.size term in
    { width = w; height = h }

  let handle_input state market_data = function
    | `Key (`ASCII 'q', _) | `Key (`Escape, _) ->
        { state with should_exit = true }
    | `Key (`Tab, _) ->
        { state with focus = match state.focus with Chart -> MarketOverview | MarketOverview -> Chart }
    | `Key (`Arrow `Left, _) when state.focus = Chart ->
        { state with chart_state = 
            { state.chart_state with 
              start_index = max 0 (state.chart_state.start_index - 1) 
            }
        }
    | `Key (`Arrow `Right, _) when state.focus = Chart ->
        { state with chart_state = 
            { state.chart_state with 
              start_index = state.chart_state.start_index + 1
            }
        }
    | `Key (`Arrow `Up, _) when state.focus = MarketOverview ->
      let pairs = Market_data.default_pairs in
      let current_idx = 
        match state.pair with
        | p -> 
            match List.find_index (fun pair -> pair.base = p.base) pairs with
            | Some idx -> idx
            | None -> 0
      in
      let new_idx = if current_idx > 0 then current_idx - 1 
        else List.length pairs - 1 in
      { state with pair = List.nth pairs new_idx }
  
  | `Key (`Arrow `Down, _) when state.focus = MarketOverview ->
      let pairs = Market_data.default_pairs in
      let current_idx = 
        match state.pair with
        | p -> 
            match List.find_index (fun pair -> pair.base = p.base) pairs with
            | Some idx -> idx
            | None -> 0
      in
      let new_idx = if current_idx < List.length pairs - 1 
        then current_idx + 1 else 0 in
      { state with pair = List.nth pairs new_idx }    
    | `Key (`Arrow `Left, _) ->
        let current_idx = List.find_index (fun p -> p = state.pair) Market_data.default_pairs 
          |> Option.value ~default:0 in
        let new_idx = if current_idx > 0 then current_idx - 1 
          else List.length Market_data.default_pairs - 1 in
        { state with pair = List.nth Market_data.default_pairs new_idx }
    | `Key (`Arrow `Right, _) ->
        let current_idx = List.find_index (fun p -> p = state.pair) Market_data.default_pairs 
          |> Option.value ~default:0 in
        let new_idx = if current_idx < List.length Market_data.default_pairs - 1 
          then current_idx + 1 else 0 in
        { state with pair = List.nth Market_data.default_pairs new_idx }
    | `Key (`ASCII '1', _) when state.focus = Chart ->
        { state with chart_state = 
            { state.chart_state with timeframe = OneHour; start_index = 0 }
        }
    | `Key (`ASCII '4', _) when state.focus = Chart ->
        { state with chart_state = 
            { state.chart_state with timeframe = FourHour; start_index = 0 }
        }
    | `Key (`ASCII 'd', _) when state.focus = Chart ->
        { state with chart_state = 
            { state.chart_state with timeframe = OneDay; start_index = 0 }
        }
    | `Key (`ASCII 'w', _) when state.focus = Chart ->
        { state with chart_state = 
            { state.chart_state with timeframe = OneWeek; start_index = 0 }
        }
    | `Key (`ASCII 'r', _) ->
        let _ = Market_data.update_ticker market_data state.pair.base in
        state
    | _ -> state

  let log_terminal msg =
  Lwt_io.printl ("[TERMINAL] " ^ msg)

let rec run_loop state market_data =
  let open Lwt.Syntax in
  if state.should_exit then
    Lwt.return_unit
  else
    let image = render state market_data in  (* Pass market_data to render *)
    let* () = Lwt.return (Notty_unix.Term.image term image) in
    
    (* Add logging for market data updates *)
    let* () = log_terminal "Updating market data..." in
    let* state = 
      let* result = Market_data.update_ticker market_data state.pair.base in
      match result with
      | Ok ticker ->
          let* () = log_terminal (Printf.sprintf "Update successful: price=%.2f" ticker.price) in
          Lwt.return { state with data = Some ticker }
      | Error err -> 
          let* () = log_terminal (Printf.sprintf "Update failed: %s" err) in
          Lwt.return state
    in
    
    let* ev = Lwt.return (Notty_unix.Term.event term) in
    match ev with
    | `Resize (w, h) ->
        run_loop { state with size = { width = w; height = h } } market_data
    | `Mouse _ | `Paste _ ->
        run_loop state market_data
    | `Key _ as key_event ->
        run_loop (handle_input state market_data key_event) market_data
    | `End ->
        Lwt.return_unit

  let initialize () : (unit, error) result Lwt.t =
    let open Lwt.Syntax in
    Lwt.catch
      (fun () ->
        let size = get_terminal_size () in
        let market_data = Market_data.create () in
        let initial_pair = List.hd Market_data.default_pairs in
        let* result = Market_data.update_ticker market_data initial_pair.base in
        let initial_data = match result with
          | Ok ticker -> Some ticker
          | Error _ -> None
        in
        
        let initial_state = {
          size;
          pair = initial_pair;
          data = initial_data;
          focus = Chart;
          should_exit = false;
          chart_state = {
            timeframe = OneHour;
            start_index = 0;
            data = [];
          };
        } in
        
        let* () = Lwt_io.printl "Initializing terminal..." in
        let* () = run_loop initial_state market_data in
        let* () = Lwt_io.printl "Shutting down..." in
        Lwt.return_ok ())
      (fun exn -> 
        Lwt.return_error (Printexc.to_string exn))
end
