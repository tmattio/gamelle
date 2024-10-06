module Tsdl_image = Tsdl_image.Image
open Gamelle_common
open Common

type s = { bmp : Sdl.texture; bmp_x : int; bmp_y : int; w : int; h : int }
type t = (io, s) Delayed.t

let size t = (t.w, t.h)

let of_texture ~io bmp =
  let& bmp = Sdl.create_texture_from_surface io.backend.renderer bmp in
  let& _, _, (w, h) = Sdl.query_texture bmp in
  { bmp; bmp_x = 0; bmp_y = 0; w; h }

let sub t x y w h =
  Delayed.make @@ fun ~io ->
  let t = Delayed.force ~io t in
  let renderer = io.backend.renderer in
  let target_texture =
    Sdl.create_texture renderer Sdl.Pixel.format_rgba8888
      Sdl.Texture.access_target ~w ~h
  in
  match target_texture with
  | Error (`Msg e) -> failwith ("Failed to create target texture: " ^ e)
  | Ok target_texture ->
      let original_target = Sdl.get_render_target renderer in
      let _ = Sdl.set_render_target renderer (Some target_texture) in
      let src_rect = Sdl.Rect.create ~x:(t.bmp_x + x) ~y:(t.bmp_y + y) ~w ~h in
      let dst_rect = Sdl.Rect.create ~x:0 ~y:0 ~w ~h in
      let _ = Sdl.render_copy renderer t.bmp ~src:src_rect ~dst:dst_rect in
      let _ = Sdl.set_render_target renderer original_target in
      { bmp = target_texture; bmp_x = 0; bmp_y = 0; w; h }

let load binstring =
  Delayed.make @@ fun ~io ->
  let rw = Sdl_buffer.load ~io binstring in
  let& bmp = Tsdl_image.load_rw (Sdl_buffer.get rw) true in
  let _ = Sys.opaque_identity rw in
  of_texture ~io bmp

let free ~io t =
  let t = Delayed.force ~io t in
  Sdl.destroy_texture t.bmp
