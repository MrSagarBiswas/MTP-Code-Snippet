open Effect
open Effect.Deep

type 'a _promise = Done of 'a
                | Error of exn
                | Waiting of ('a, unit) continuation list
                
type 'a promise = 'a _promise ref

type _ Effect.t += Async : ('a -> 'b) * 'a -> 'b promise Effect.t
type _ Effect.t += Await : ('a promise) -> 'a Effect.t
type _ Effect.t += Yield : unit -> unit Effect.t

let async f v = perform (Async (f, v))
let await p = perform (Await p)
let yield () = perform (Yield ())

let run main =
let q = Queue.create () in
let enqueue f = Queue.push f q in
let run_next () = if Queue.is_empty q then ()
                else (Queue.pop q) (); in
let rec fork : type a b. b promise -> (a -> b) -> a -> unit =
    fun p f v ->
        match_with f v {
            retc = (fun v ->
                match !p with
                | Waiting l ->
                List.iter (fun k -> continue k v) l;
                p := Done v;
                run_next ()
                | _ -> run_next();
            );
            exnc = (fun e -> 
                match !p with
                | Waiting l ->
                List.iter (fun k -> discontinue k e) l;
                p := Error e;
                run_next ()
                | _ -> run_next();
            );
            effc = (fun (type b)(eff: b Effect.t) ->
                match eff with
                    | Async (f, v) -> Some (
                        fun (k: (b, _) continuation) ->
                            let new_p = ref (Waiting []) in
                            enqueue (fun () -> continue k new_p);
                            fork new_p f v
                        )
                    | Await pm -> Some (
                        fun k -> match !pm with
                            | Done v -> continue k v
                            | Waiting l -> pm:= Waiting (k::l); run_next ()
                            | Error e -> discontinue k e
                        )
                    | Yield () -> Some (
                        fun k -> enqueue (fun () -> continue k ()); run_next ()
                        )
                    | _ -> None
            )
        } in
fork (ref (Waiting [])) main ();;



let fun1 ()= 
    print_endline "fun1 part-1\n";
    yield ();
    print_endline "fun1 part-2\n";;
    
let fun2 ()= print_endline "fun2\n";;

let main () =
    print_endline "Yield Experiment: \n\nmain is Starting...\n";
    let _ = async fun1 () in 
    let _ = async fun2 () in
    print_endline "main is Finishing.."
    
let () = run main;;

print_newline ();; print_newline ();;

let func (name, v) = 
    print_endline (name ^ " is Starting...\n");
    let x = ref 0 in
        for i=1 to v do 
            Printf.printf "%s : %d\n%!" name i;
            x:=!x+1;
            if !x mod 3 = 0 then yield ()
        done;
    print_endline (name ^ " is Finishing...\n");
    !x;;
    

let main () = 
    print_endline "Async/Await Experiment: \n\nmain is Starting...\n";
    let x = async func ("X", 7) in
    let y = async func ("Y", 10) in
    print_int ((await y) + (await x));
    print_endline "\nmain is Finishing..."
    
let () = run main;;
    