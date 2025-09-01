open Ctypes
open Foreign
open PosixTypes

let time = foreign "time" (ptr time_t @-> returning time_t)
let cur_time = time (from_voidp time_t null)

let time' () = time (from_voidp time_t null)

let difftime = foreign "difftime" (time_t @-> time_t @-> returning double)

let ctime = foreign "ctime" (ptr time_t @-> returning string)

let ctime' () = ctime (time' ())

let () = Printf.printf "%s" (ctime' ())

let time_in_ocaml () = let t = time' () in 
  coerce time_t int64_t t

let () = let t = time_in_ocaml () in
  let str = Int64.to_string t in
  print_endline str

  

let delta = 
  let t1 = time' () in 
  Unix.sleep 2;
  let t2 = time' () in
  difftime t2 t1;;

print_endline (string_of_float delta)