open Effect
open Effect.Deep

module Priority_Queue = struct
  type 'a t = { mutable heap : (int * 'a) array; mutable size : int }

  let create () =
    { heap = Array.make 16 (0, Obj.magic ()); size = 0 }

  let parent i = (i - 1) / 2
  let left i = 2 * i + 1
  let right i = 2 * i + 2

  let swap arr i j =
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp

  let resize pq =
    let n = Array.length pq.heap in
    let new_heap = Array.make (2 * n) (0, Obj.magic ()) in
    Array.blit pq.heap 0 new_heap 0 pq.size;
    pq.heap <- new_heap

  let rec heapify_up pq i =
    if i > 0 then
      let p = parent i in
      if fst pq.heap.(i) > fst pq.heap.(p) then (
        swap pq.heap i p;
        heapify_up pq p
      )

  let rec heapify_down pq i =
    let l = left i in
    let r = right i in
    let largest = ref i in
    if l < pq.size && fst pq.heap.(l) > fst pq.heap.(!largest) then
      largest := l;
    if r < pq.size && fst pq.heap.(r) > fst pq.heap.(!largest) then
      largest := r;
    if !largest <> i then (
      swap pq.heap i !largest;
      heapify_down pq !largest
    )

  let push pq priority x =
    if pq.size = Array.length pq.heap then resize pq;
    pq.heap.(pq.size) <- (priority, x);
    pq.size <- pq.size + 1;
    heapify_up pq (pq.size - 1)

  let pop pq =
    if pq.size = 0 then None
    else
      let root = pq.heap.(0) in
      pq.size <- pq.size - 1;
      pq.heap.(0) <- pq.heap.(pq.size);
      heapify_down pq 0;
      Some (snd root)

  let is_empty pq = pq.size = 0
end


type 'a _promise =
  | Done of 'a
  | Error of exn
  | Waiting of (('a, unit) continuation * int) list

type 'a promise = 'a _promise ref

type _ Effect.t += Async : ('a -> 'b) * 'a * int -> 'b promise Effect.t
type _ Effect.t += Await : 'a promise -> 'a Effect.t
type _ Effect.t += Yield : unit -> unit Effect.t

let async ?(priority = 0) f v = perform (Async (f, v, priority))
let await p = perform (Await p)
let yield () = perform (Yield ())


let run main =
  let pq = Priority_Queue.create () in
  let enqueue priority f = Priority_Queue.push pq priority f in
  let current_priority = ref 0 in

  let run_next () =
    match Priority_Queue.pop pq with
    | None -> ()
    | Some f -> 
        let saved_priority = !current_priority in
        f ();
        current_priority := saved_priority
  in

  let rec fork : type a b. b promise -> (a -> b) -> a -> unit =
   fun p f v ->
    match_with f v
      {
        retc =
          (fun v ->
            match !p with
            | Waiting l ->
                List.iter
                  (fun (k, pri) ->
                    Priority_Queue.push pq pri
                      (fun () ->
                        current_priority := pri;
                        continue k v))
                  l;
                p := Done v;
                run_next ()
            | _ -> run_next ());
        exnc =
          (fun e ->
            match !p with
            | Waiting l ->
                List.iter
                  (fun (k, pri) ->
                    Priority_Queue.push pq pri
                      (fun () ->
                        current_priority := pri;
                        discontinue k e))
                  l;
                p := Error e;
                run_next ()
            | _ -> run_next ());
        effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Async (f, v, pri) ->
                Some
                  (fun (k : (b, _) continuation) ->
                    let new_p = ref (Waiting []) in
                    let current_pri = !current_priority in
                    enqueue pri (fun () -> 
                      current_priority := pri;
                      fork new_p f v);
                    continue k new_p)
            | Await pm ->
                Some
                  (fun k ->
                    match !pm with
                    | Done v -> continue k v
                    | Waiting l ->
                        pm := Waiting ((k, !current_priority) :: l);
                        run_next ()
                    | Error e -> discontinue k e)
            | Yield () ->
                Some
                  (fun k ->
                    let task_priority = !current_priority in
                    enqueue task_priority (fun () ->
                      current_priority := task_priority;
                      continue k ());
                    run_next ())
            | _ -> None);
      }
  in
  fork (ref (Waiting [])) main ()


let fun1 () =
  print_endline "fun1 part-1\n";
  yield ();
  print_endline "fun1 part-2\n"

let fun2 () = print_endline "fun2\n";;

let main1 () =
  print_endline "Yield Experiment: \n\nmain is Starting...\n";
  let _ = async ~priority:5 fun1 () in
  let _ = async ~priority:1 fun2 () in
  print_endline "main is Finishing..";

let () = run main1;;

print_newline (); print_newline ();;

let func (name, v) =
  print_endline (name ^ " is Starting...\n");
  let x = ref 0 in
  for i = 1 to v do
    Printf.printf "%s : %d\n%!" name i;
    x := !x + 1;
    if !x mod 3 = 0 then yield ()
  done;
  print_endline (name ^ " is Finishing...\n");
  !x

let main2 () =
  print_endline "Async/Await Experiment: \n\nmain is Starting...\n";
  let x = async ~priority:2 func ("X", 7) in
  let y = async ~priority:10 func ("Y", 10) in
  print_int ((await y) + (await x));
  print_endline "\nmain is Finishing..."

let () = run main2
