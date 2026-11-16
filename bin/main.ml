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

let () = render 0 0 matrix_size
