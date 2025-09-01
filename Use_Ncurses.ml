open Ctypes
type window = unit ptr
let window: window typ = ptr void

open Foreign
let ncurses = Dl.dlopen ~filename:"libncurses.so.6" ~flags:[Dl.RTLD_NOW]
let foreign = foreign ~from:ncurses

let initscr = foreign "initscr" (void @-> returning window)
let newwin = foreign "newwin" (int @-> int @-> int @-> int @-> returning window)
let endwin = foreign "endwin" (void @-> returning void)
let refresh = foreign "refresh" (void @-> returning void)
let wrefresh = foreign "wrefresh" (window @-> returning void)
let addstr = foreign "addstr" (string @-> returning void)
let mvwaddch = foreign "mvwaddch" (window @-> int @-> int @-> char @-> returning void)
let mvwaddstr = foreign "mvwaddstr" (window @-> int @-> int @-> string @-> returning void)
let box = foreign "box" (window @-> char @-> char @-> returning void)
let cbreak = foreign "cbreak" (void @-> returning int)

let rec printInTerminal win h w str i =
  if i > String.length str then () else(
    mvwaddstr win h w (String.sub str 0 i);
    box win '\000' '\000';
    wrefresh win;
    Unix.sleep 1;
    printInTerminal win h w str (i+1)
  )

let () =
  let main_window = initscr () in
  ignore (cbreak());
  let small_window = newwin 10 20 5 30 in
  mvwaddstr main_window 2 34 "NCURSES DEMO";
  refresh ();

  printInTerminal small_window 4 4 "Hello World!" 1;

  Unix.sleep 5;
  endwin ()