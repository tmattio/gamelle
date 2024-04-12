open Gamelle

type pos = float * float

type state = {
  bmp : Bitmap.t;
  scale : float;
  mouse : pos;
  click : pos option;
  rects : (pos * pos) list;
}

let fresh_state file =
  {
    bmp = Bitmap.load file;
    scale = 1.;
    mouse = (0., 0.);
    click = None;
    rects = [];
  }

let compute_rect ((x1, y1), (x2, y2)) =
  (* some strong assumptions here *)
  let w = x2 -. x1 in
  let h = y2 -. y1 in
  Box.v (Point.v x1 y1) (Size.v w h)

let main ~io st =
  let x, y = Vec.to_tuple @@ Event.mouse_pos ~io in
  let mouse = (x /. st.scale, y /. st.scale) in
  let st = { st with mouse } in
  let st =
    match (st.click, Event.is_pressed ~io `click_left) with
    | None, false ->
        if Event.is_pressed ~io `wheel then
          let amount = Event.wheel_delta ~io in
          { st with scale = st.scale +. amount }
        else st
    | Some _, true -> st (* just keep dragging *)
    | None, true -> { st with click = Some st.mouse }
    | Some fst_pos, false ->
        { st with click = None; rects = (fst_pos, st.mouse) :: st.rects }
  in
  fill_rect ~io ~color:Color.black (Box.v (Point.v 0. 0.) (Point.v 500. 500.));
  show_cursor true;
  let io = View.scaled st.scale io in
  draw ~io st.bmp (Point.v 0. 0.);
  Option.iter
    (fun pos ->
      let rect = compute_rect (pos, st.mouse) in
      draw_rect ~io ~color:Color.green rect)
    st.click;
  let pale_green = Color.(with_a green 0.2) in
  List.iter
    (fun poss ->
      let rect = compute_rect poss in
      fill_rect ~io ~color:pale_green rect)
    st.rects;
  st

let print_rects ~file { rects; _ } =
  let oc = open_out file in
  List.iter
    (fun poss ->
      let rect = compute_rect poss in
      let x, y = Vec.to_tuple (Box.o rect) in
      let w, h = Vec.to_tuple (Box.size rect) in
      Printf.fprintf oc "%.0f %.0f %.0f %.0f\n%!" x y w h)
    rects;
  close_out oc

let do_the_thing out file =
  let st = fresh_state file in
  run st @@ fun ~io st ->
  if Event.is_pressed ~io `quit then print_rects ~file:out st;
  main ~io st
