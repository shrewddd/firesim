let seed = 3141592653589793238
let random = Random.init seed
let matrix_size = 40

let forest_matrix =
  Array.init_matrix matrix_size matrix_size (fun _ _ -> Random.int 2)

let get_emoji n = if n = 0 then "⬛" else if n = 1 then "🟩" else "🔥"

let rec render r c size =
  if r < size then
    if c < size then begin
      print_string (get_emoji forest_matrix.(r).(c));
      render r (c + 1) size
    end
    else begin
      print_newline ();
      render (r + 1) 0 size
    end

let first_row = forest_matrix.(0)
let set_on_fire n = if n = 1 then 2 else n
let ignite_first_row = Array.map_inplace set_on_fire first_row
let burning_trees = Queue.create ()

let fill_queue =
  Array.iteri
    (fun i x -> if x = 2 then Queue.push (0, i) burning_trees)
    first_row

let rec simulate_fire (q : (int * int) Queue.t) = if not (Queue.is_empty q) then 
  let (r, c) = Queue.pop q in
  if r + 1 >= 0 && c     >= 0 && r + 1 < Array.length forest_matrix && c     < Array.length forest_matrix.(0) && forest_matrix.(r + 1).(c) = 1 then begin forest_matrix.(r + 1).(c) <- 2; Queue.push (r + 1, c) q end;
  if r - 1 >= 0 && c     >= 0 && r - 1 < Array.length forest_matrix && c     < Array.length forest_matrix.(0) && forest_matrix.(r - 1).(c) = 1 then begin forest_matrix.(r - 1).(c) <- 2; Queue.push (r - 1, c) q end;
  if r >= 0     && c + 1 >= 0 && r     < Array.length forest_matrix && c + 1 < Array.length forest_matrix.(0) && forest_matrix.(r).(c + 1) = 1 then begin forest_matrix.(r).(c + 1) <- 2; Queue.push (r, c + 1) q end;
  if r >= 0     && c - 1 >= 0 && r     < Array.length forest_matrix && c - 1 < Array.length forest_matrix.(0) && forest_matrix.(r).(c - 1) = 1 then begin forest_matrix.(r).(c - 1) <- 2; Queue.push (r, c - 1) q end;
  simulate_fire q

let () =
  ignite_first_row;
  fill_queue;
  render 0 0 matrix_size;
  simulate_fire burning_trees;
  print_newline ();
  print_newline ();
  render 0 0 matrix_size;

