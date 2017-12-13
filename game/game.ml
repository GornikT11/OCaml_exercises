(* Tip, ki predstavlja izid igre. *)
type result = Victory of Field.symbol | Draw

(* Funkcija za zamenjavo igralcov. *)
let switch_symbol = function
  | Field.Circle -> Field.Cross
  | Field.Cross -> Field.Circle
  | Field.Empty -> Field.Empty
  
  (*/// Napiši funkcijo, ki pretvori Cricle v Cross, Cross v Circle in
        vrednost Empty pusti nespremenjeno. Pri tem ne pozabi, da so ti
        tipi definirani v modulu Field. ///*)

(* Funkcija, ki zahteva vhod igralca in poskusi narediti potezo. Pri tem
   vse neveljavne poteze prestreže. *)
let rec make_move player field =
  print_endline ("It is the turn of : " ^ (Field.symbol_to_string player));

  (*/// Izpiši kdo je na potezi. Vhod "player" je tipa Field.symbol. ///*)
  try
    print_string "Postavi v vrsto : ";
    let x = read_int () in
    print_string "Postavi v stolpec : ";
    let y = read_int () in
    (*/// Nastavi simbol na koordinatah x in y v polju field na znak igralca
          player. ///*)
	Field.set_symbol x y player field
  with
  (* Prestrezi in obravnavaj napake. *)
  (*/// Obravnavaj napake tako, da izpišeš sporočilo uporabniku in ponoviš
        potezo. Potezo ponoviš tako, da ponovno kličeš "make_move". ///*)
  | Field.Coordinate_out_of_bounds ->
    print_endline "S temi koordinatami si zgrešil igralno ploščo";
	make_move player field
  | Field.Nonempty_value_at_coordinates ->
    print_endline "tu je že nekdo igral";
	make_move player field
  | Failure msg when msg = "int_of_string" ->
    print_endline "Koordinate morajo biti celoštevilske";
    make_move player field
	
	
(* Glavna zanka igre, ki izvaja poteze igralcev dokler se igra ne konča.
   Hkrati vsako potezo izpiše trenutno stanje. *)
let rec game_loop player field =
  match Field.victory field with
  | Some x -> Victory x
  | None -> if Field.no_more_moves field then Draw 
            else 
			  let new_field = make_move player field in
			  print_newline (); Field.print_field new_field; print_newline ();
			  let new_player = switch_symbol player in
			  game_loop new_player new_field
			
			
  (*/// Vhod "player" predstavlja igralca, ki je na potezi in "field" trenutno
        stanje igralnega polja.
        Najprej preveri, če je kdo že zmagal in v tem primeru vrni pravilen
        rezultat (s pomočjo tipa, ki smo ga definirali na začetku datoteke).
        Nato preveri ali je sploh še kakšna možna poteza. Če ni, vrni
        rezultat, ki predstavlja neodločen izid.
        Sicer pa uporabi spodnjo zakomentiratno kodo, da se izvede naslednja
        poteza. ///*)

  (*
      (* Make a move. *)
      let new_field = make_move player field in
      (* Print field. *)
      print_newline (); Field.print_field new_field; print_newline ();
      (* Switch player and repeat loop. *)
      let new_player = switch_symbol player in
      game_loop new_player new_field
  *)

(* Zažene igro na praznem polju. Po končani igri izpiše izzi in zažene novo
   igro. *)
let rec run_game () =
  let field = Field.empty_field () in
  let current_player = Field.Cross in
  match game_loop current_player field with
  | Draw ->
    print_endline "Igra je bila neodločena. Začnimo znova!";
    print_newline ();
    run_game ()
  | Victory s ->
    print_endline ("Igralec "^(Field.symbol_to_string s)^" je zmagal! Začnimo znova!");
    print_newline ();
    run_game ()

(* ===== ZAŽENI IGRO ===== *)
(* Odkomentiraj naslednji del v primeru, ko želiš, da se igra zažene takoj,
   ko uporabiš datoteko (uporabno pri prevedenih datotekah). *)


let () = run_game ()

