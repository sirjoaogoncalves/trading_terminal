(* lib/types.ml *)
type api_error =
  | NetworkError of string
  | ParseError of string

type error = string

type terminal_size = {
  width: int;
  height: int;
}

type market_pair = {
  base: string;
  quote: string;
}

type market_data = {
  price: float;
  volume_24h: float;
  change_24h: float;
  market_cap: float option;
  last_updated: string;
}

type chart_timeframe =
  | OneHour
  | FourHour
  | OneDay
  | OneWeek

type chart_state = {
  timeframe: chart_timeframe;
  start_index: int;
  data: market_data list;
}

type focus = Chart | MarketOverview

type state = {
  size: terminal_size;
  pair: market_pair;
  data: market_data option;
  focus: focus;
  should_exit: bool;
  chart_state: chart_state;
}

type candle = {
  time: float;
  open_price: float;
  high: float;
  low: float;
  close: float;
  volume: float;
}

type chart_data = candle list
