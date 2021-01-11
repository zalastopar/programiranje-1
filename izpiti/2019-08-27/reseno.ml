(*1. naloga *)

(*1.a*)
let odstej_trojici t1 t2 = 
  let (a1, b1, c1) = t1 in
  let (a2, b2, c2) = t2 in 
  (a1 + a2, b1 + b2, c1 + c2)
  
(*ali*)
let odstej_trojici2 (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)



(*1.b*)
let max_rezultat_do_n f n =
  let rec pomozna f n max =
    if n < 0 then max
    else if (f n) > max then pomozna f (n-1) (f n) 
    else pomozna f (n-1) max
  in
  pomozna f n (f n)

(*lahko probas List.init*)

(*1.c*)

(*tale ni repno rekurzivna*)
let rec pocisti_seznam_1 = function
    | [] -> []
    | (Some x) :: xs -> x :: pocisti_seznam_1 xs
    | _ :: xs -> pocisti_seznam_1 xs

let rec pocisti_seznam l = 
  let rec pomozna acc = function
    | [] -> List.rev acc
    | (Some x) :: xs -> pomozna (x::acc) xs
    | _ :: xs -> pomozna acc xs
  in pomozna [] l
(*1.d*)
let rec narascanje = function
    | [] -> true
    | a :: b :: rest -> if a > b then false else narascanje (b :: rest)
    | a :: [] -> true

let rec padanje = function
  | [] -> true
  | a :: b :: rest -> if b > a then false else padanje (b :: rest)
  | a :: [] -> true

let preveri_urejenost sez =
  let rec pomozna sez sode lihe = match sez with
    | [] -> (narascanje sode) && (padanje lihe)
    | a :: rest -> if (a mod 2) = 0 then pomozna rest (sode @ [a]) lihe
      else pomozna rest sode (lihe @ [a])
  in pomozna sez [] []

(*lahko tudi*)
let preveri_urejenost2 l =
  let rec pomozna min_sodo max_liho = function
    | [] -> true
    | x :: xs -> if (x mod 2) = 0 then x > min_sodo && pomozna x max_liho xs
    else x < max_liho && pomozna min_sodo x xs
  in
  pomozna (-9999) (9999) l



(*2. naloga*)
type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list

(*2.a*)
let gnezdenje_primer = 
  Podseznam [Element 1; Element 2; 
  Podseznam (  
    [Element 3; Podseznam [Element 4]; Podseznam []]
    );
  Podseznam [Element 5]]

(*2.b*)
let rec najvecja_globina g = 
  match g with
  | Element _ -> 0
  | Podseznam xs -> 1 + (List.fold_left max 1 (list.map najvecja_globina xs))

let najvecja_globina g_list = 
(*2.c*)
let rec preslikaj f g = match g with
  | Element x -> Element (f x)
  | Podseznam xs -> Podseznam ((List.map (preslikaj f)) xs)

let splosci = function
  | Element x -> [x]
  | Podseznam x ->
    let splosceni = List.map splosci xs in
    (*List.concat splosceni*)
    List.fold_left (@) [] splosceni

let rec alternirajoci_konstruktorji = function
  | [] -> true
  | [x] -> true
  | Element _ :: Podseznam p :: xs -> alternirajoci_konstruktorji ((Podseznam p)::xs)
  | Podseznam _ :: Element p :: xs -> alternirajoci_konstruktorji ((Element p)::xs)
  | _ -> false


let rec zlozi_gnezdenje f acc g = 
  | Element x -> f acc x
  | Podseznam l -> 

let zlozi_preko_gnezdenja f acc g =
  1



