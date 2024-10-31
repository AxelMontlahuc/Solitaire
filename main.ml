type couleur = Coeur | Carreau | Trefle | Pique
type valeur = Val_As | Val_2 | Val_3 | Val_4 | Val_5 | Val_6 | Val_7 | Val_8 | Val_9 | Val_10 | Val_Valet | Val_Dame | Val_Roi
type carte = { valeur : valeur; couleur : couleur; face_up : bool }
type etat_jeu = {
  tableau : carte list list;        
  fondations : carte list list;     
  pioche : carte list;              
  reserve : carte list;           
}
type destination =
  | Reserve
  | Colonne of int

let rec split_n n lst =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h::t ->
        let (l1, l2) = split_n (n - 1) t in
        (h :: l1, l2)

let creer_paquet () =
  let couleurs = [Coeur; Carreau; Trefle; Pique] in
  let valeurs = [Val_As; Val_2; Val_3; Val_4; Val_5; Val_6; Val_7; Val_8; Val_9; Val_10; Val_Valet; Val_Dame; Val_Roi] in
  let cartes = List.flatten (List.map (fun couleur ->
    List.map (fun valeur -> { valeur; couleur; face_up = false }) valeurs
  ) couleurs) in
  Random.self_init ();
  List.sort (fun _ _ -> Random.int 3 - 1) cartes

let distribuer_cartes paquet =
  let rec distribuer acc paquet n =
    if n < 1 then (List.rev acc, paquet)
    else
      let (colonne, reste) = split_n n paquet in
      let carte_face_up = { (List.hd colonne) with face_up = true } in
      let colonne = carte_face_up :: (List.tl colonne) in
      distribuer (colonne :: acc) reste (n - 1)
  in
  distribuer [] paquet 7

let initialiser_jeu () =
  let paquet = creer_paquet () in
  let (tableau, reste_paquet) = distribuer_cartes paquet in
  {
    tableau;
    fondations = [ []; []; []; [] ];
    pioche = reste_paquet;
    reserve = [];
  }

let string_of_couleur c =
  match c with
  | Coeur -> "\027[31m♥\027[0m]"
  | Carreau -> "\027[31m♦\027[0m]"
  | Trefle -> "♣]"
  | Pique -> "♠]"

let string_of_valeur v =
  match v with
  | Val_As -> "[ A"
  | Val_Valet -> "[ J"
  | Val_Dame -> "[ Q"
  | Val_Roi -> "[ K"
  | Val_10 -> "[10"
  | Val_2 -> "[ 2"
  | Val_3 -> "[ 3"
  | Val_4 -> "[ 4"
  | Val_5 -> "[ 5"
  | Val_6 -> "[ 6"
  | Val_7 -> "[ 7"
  | Val_8 -> "[ 8"
  | Val_9 -> "[ 9"

let afficher_carte carte =
  if carte.face_up then
    Printf.printf "%-2s     " (Printf.sprintf "%s%s" (string_of_valeur carte.valeur) (string_of_couleur carte.couleur))
  else
    Printf.printf "%-2s     " "[■■■]"

let afficher_tableau tableau =
  let max_col_length = List.fold_left (fun acc col -> max acc (List.length col)) 0 tableau in
  for row = 0 to max_col_length - 1 do
    List.iter (fun colonne ->
      let shift = max_col_length - List.length colonne in
      let idx = row - shift in
      if idx >= 0 then
        afficher_carte (List.nth colonne idx)
      else
        Printf.printf "%-2s        " ""
    ) tableau;
    print_newline ()
  done;
  List.iteri (fun i _ -> Printf.printf "  %-2d      " (i+1)) tableau;
  print_newline ()

let afficher_jeu etat =
  Printf.printf "\nFondations: ";
  List.iteri (fun i pile ->
    Printf.printf "[%d]: " (i+1);
    match pile with
    | [] -> Printf.printf "Empty  "
    | carte::_ -> afficher_carte carte; Printf.printf "  "
  ) etat.fondations;
  Printf.printf "\nPioche: %d cartes   " (List.length etat.pioche);
  Printf.printf "Réserve: ";
  (match etat.reserve with
  | [] -> Printf.printf "Empty\n"
  | carte::_ -> afficher_carte carte; print_newline ());
  Printf.printf "\nTableau:\n";
  afficher_tableau etat.tableau

let parse_colonne str =
  match str with
  | "0" -> Some Reserve
  | _ ->
      try
        let index = int_of_string str - 1 in
        if index >= 0 && index < 7 then Some (Colonne index) else None
      with Failure _ -> None

let deplacement_valide carte_source carte_dest =
  let couleurs_alternent c1 c2 =
    match c1, c2 with
    | (Coeur | Carreau), (Trefle | Pique) -> true
    | (Trefle | Pique), (Coeur | Carreau) -> true
    | _ -> false
  in
  let valeur_suivante v1 v2 =
    let valeur_int v =
      match v with
      | Val_As -> 1
      | Val_2 -> 2
      | Val_3 -> 3
      | Val_4 -> 4
      | Val_5 -> 5
      | Val_6 -> 6
      | Val_7 -> 7
      | Val_8 -> 8
      | Val_9 -> 9
      | Val_10 -> 10
      | Val_Valet -> 11
      | Val_Dame -> 12
      | Val_Roi -> 13
    in
    valeur_int v1 = valeur_int v2 - 1
  in
  couleurs_alternent carte_source.couleur carte_dest.couleur &&
  valeur_suivante carte_source.valeur carte_dest.valeur

let refill_pioche etat =
  if etat.pioche = [] && etat.reserve <> [] then
    { etat with pioche = List.rev etat.reserve; reserve = [] }
  else
    etat

let piocher_carte etat =
  match etat.pioche with
  | [] ->
      refill_pioche etat
  | carte::reste_pioche ->
      let carte_face_up = { carte with face_up = true } in
      { etat with pioche = reste_pioche; reserve = carte_face_up :: etat.reserve }

let deplacer_carte etat source_str dest_str =
  try
    let source = parse_colonne source_str in
    let dest = parse_colonne dest_str in
    match source, dest with
    | Some (Colonne s_idx), Some (Colonne d_idx) ->
        if s_idx = d_idx then (
          Printf.printf "La source et la destination sont identiques.\n";
          etat
        ) else
          let source_col = List.nth etat.tableau s_idx in
          let dest_col = List.nth etat.tableau d_idx in
                    let rec split_hidden_visible lst =
            match lst with
            | [] -> ([], [])
            | ({ face_up = false } as c) :: rest ->
                let (hidden, visible) = split_hidden_visible rest in
                (c :: hidden, visible)
            | visible -> ([], visible)
          in
          let hidden_cards, visible_cards = split_hidden_visible source_col in
                    let rec find_valid_move acc cards =
            match cards with
            | [] -> None
            | card :: rest ->
                let deplacement_possible =
                  match dest_col with
                  | [] -> card.valeur = Val_Roi
                  | dest_card :: _ -> deplacement_valide card dest_card
                in
                if deplacement_possible then
                  Some (List.rev (card :: acc), rest)
                else
                  find_valid_move (card :: acc) rest
          in
          begin match find_valid_move [] visible_cards with
          | Some (move_stack, remaining_visible) ->
              let new_source_col = List.rev (hidden_cards @ List.rev remaining_visible) in
              let new_dest_col = move_stack @ dest_col in
              let new_source_col =
                match new_source_col with
                | { face_up = false } as c :: rest -> { c with face_up = true } :: rest
                | _ -> new_source_col
              in
              let new_tableau = List.mapi (fun i col ->
                if i = s_idx then new_source_col
                else if i = d_idx then new_dest_col
                else col
              ) etat.tableau in
              { etat with tableau = new_tableau }
          | None ->
              Printf.printf "Déplacement invalide.\n";
              etat
          end
    | Some Reserve, Some (Colonne d_idx) ->
        (match etat.reserve with
        | [] ->
            Printf.printf "La réserve est vide.\n";
            etat
        | carte::reste_reserve ->
            let dest_col = List.nth etat.tableau d_idx in
            let deplacement_possible =
              match dest_col with
              | [] -> carte.valeur = Val_Roi
              | carte_dest::_ -> deplacement_valide carte carte_dest
            in
            if deplacement_possible then
              let new_dest_col = carte :: dest_col in
              let new_tableau = List.mapi (fun i col ->
                if i = d_idx then new_dest_col else col
              ) etat.tableau in
              { etat with tableau = new_tableau; reserve = reste_reserve }
            else (
              Printf.printf "Déplacement invalide.\n";
              etat
            )
        )
    | _ -> etat
  with _ ->
    (Printf.printf "Déplacement invalide.\n";
    etat)

let foundation_index c =
  match c with
  | Coeur -> 0
  | Carreau -> 1
  | Trefle -> 2
  | Pique -> 3

let next_val v =
  match v with
  | Val_As -> Val_2
  | Val_2 -> Val_3
  | Val_3 -> Val_4
  | Val_4 -> Val_5
  | Val_5 -> Val_6
  | Val_6 -> Val_7
  | Val_7 -> Val_8
  | Val_8 -> Val_9
  | Val_9 -> Val_10
  | Val_10 -> Val_Valet
  | Val_Valet -> Val_Dame
  | Val_Dame -> Val_Roi
  | Val_Roi -> Val_Roi  

let empiler etat source_str =
  let source = parse_colonne source_str in
  match source with
  | Some source_pos ->
      (match source_pos with
       | Reserve ->
           (match etat.reserve with
            | [] ->
                Printf.printf "La réserve est vide.\n";
                etat
            | carte::reste_reserve ->
                let foundation_idx = foundation_index carte.couleur in
                let foundation_pile = List.nth etat.fondations foundation_idx in
                let can_place =
                  match foundation_pile with
                  | [] -> carte.valeur = Val_As
                  | top_card::_ -> carte.valeur = next_val top_card.valeur
                in
                if can_place then
                  let new_foundations = List.mapi (fun i pile ->
                    if i = foundation_idx then carte :: pile else pile
                  ) etat.fondations in
                  { etat with
                    fondations = new_foundations;
                    reserve = reste_reserve
                  }
                else (
                  Printf.printf "La carte ne peut pas être empilée sur la fondation.\n";
                  etat
                )
           )
       | Colonne c_idx ->
           (match List.nth etat.tableau c_idx with
            | [] ->
                Printf.printf "La colonne est vide.\n";
                etat
            | carte::_ ->
                let foundation_idx = foundation_index carte.couleur in
                let foundation_pile = List.nth etat.fondations foundation_idx in
                let can_place =
                  match foundation_pile with
                  | [] -> carte.valeur = Val_As
                  | top_card::_ -> carte.valeur = next_val top_card.valeur
                in
                if can_place then
                  let new_foundations = List.mapi (fun i pile ->
                    if i = foundation_idx then carte :: pile else pile
                  ) etat.fondations in
                  let new_tableau = List.mapi (fun i col ->
                    if i = c_idx then (
                      match List.tl col with
                      | [] -> []
                      | new_top :: reste ->
                          if not new_top.face_up then
                            { new_top with face_up = true } :: reste
                          else
                            new_top :: reste
                    ) else col
                  ) etat.tableau in
                  { etat with
                    fondations = new_foundations;
                    tableau = new_tableau
                  }
                else (
                  Printf.printf "La carte ne peut pas être empilée sur la fondation.\n";
                  etat
                )
           )
      )
  | None ->
      Printf.printf "Source invalide.\n";
      etat

let traiter_commande etat commande =
  let mots = String.split_on_char ' ' (String.lowercase_ascii commande) in
  match mots with
  | ["deplacer"; source; dest] ->
      deplacer_carte etat source dest
  | ["empiler"; source] ->
      empiler etat source
  | ["piocher"] ->
      piocher_carte etat
  | ["help"] ->
      Printf.printf "\nCommandes disponibles:\n";
      Printf.printf "- deplacer SOURCE DEST : déplacer une carte de SOURCE vers DEST (le 0 est la réserve)\n";
      Printf.printf "- empiler SOURCE : empiler la première carte de SOURCE vers la fondation appropriée\n";
      Printf.printf "- piocher : piocher une carte depuis la pioche\n";
      Printf.printf "- quitter : quitter le jeu\n";
      Printf.printf "- help : afficher cette aide\n\n";
      etat
  | ["quitter"] ->
      exit 0
  | _ ->
      Printf.printf "Commande invalide. Tapez 'help' pour l'aide.\n";
      etat

let a_gagne etat =
  List.for_all (fun fondation ->
    match fondation with
    | [] -> false
    | _ -> List.length fondation = 13
  ) etat.fondations

let rec boucle_jeu etat =
  afficher_jeu etat;
  if a_gagne etat then (
    Printf.printf "Félicitations, vous avez gagné !\n";
    exit 0
  );
  Printf.printf "\nEntrez une commande: ";
  let commande = read_line () in
  let nouvel_etat = traiter_commande etat commande in
  boucle_jeu nouvel_etat

let () =
  let etat_initial = initialiser_jeu () in
  boucle_jeu etat_initial