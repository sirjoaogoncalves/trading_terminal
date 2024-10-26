(* lib/ui.ml *)
open Notty
open Notty.Infix

let draw_box ~title ~width ~height ~focused content =
  let border_style = if focused then A.(fg lightblue) else A.(fg white ) in
  let title_style = if focused then A.(fg lightblue ++ st bold) else A.(st bold) in
  
  let horiz = I.char border_style '-' width 1 in
  let vert = I.char border_style '|' 1 height in
  let tl = I.char border_style '+' 1 1 in
  let tr = I.char border_style '+' 1 1 in
  let bl = I.char border_style '+' 1 1 in
  let br = I.char border_style '+' 1 1 in
  
  let title_text = I.string title_style (Printf.sprintf " %s " title) in
  let title_width = String.length title + 2 in
  let left_border = I.char border_style '-' ((width - title_width) / 2) 1 in
  let right_border = I.char border_style '-' (width - title_width - ((width - title_width) / 2)) 1 in
  
  let top = tl <|> left_border <|> title_text <|> right_border <|> tr in
  let bottom = bl <|> horiz <|> br in
  let middle = vert <|> content <|> vert in
  
  top <-> middle <-> bottom

let render_keybinds ~width =
  let key_style = A.(fg black ++ bg white) in
  let desc_style = A.(fg white) in
  let spacer = I.char A.empty ' ' 2 1 in
  
  let make_key_hint key desc =
    I.string key_style (Printf.sprintf " %s " key) <|>
    I.string desc_style desc <|>
    spacer
  in
  
  let hints = [
    make_key_hint "Q" "Quit";
    make_key_hint "Tab" "Switch Focus";
    make_key_hint "<-/->" "Change Pair";
    make_key_hint "R" "Refresh";
  ] in
  
  let combined_hints = List.fold_left (<|>) I.empty hints in
  let padding = width - I.width combined_hints in
  if padding > 0 then
    combined_hints <|> I.void padding 1
  else combined_hints

let render_status_bar ~width ~message =
  let status_style = A.(fg white ++ bg blue) in
  let text = I.string status_style (Printf.sprintf " %s " message) in
  let padding = width - I.width text in
  if padding > 0 then
    text <|> I.char status_style ' ' padding 1
  else text
