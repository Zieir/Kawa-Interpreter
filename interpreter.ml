open Kawa
open Typechecker

type value =
  | VInt of int
  | VBool of bool
  | VObj of obj
  | VArray of value array
  | Null
and obj = {
  cls: string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let error s = raise (Error s)

let rec exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in

  let rec collect_attributes cls =
    let parent_attributes = match cls.parent with
      | Some parent_name ->
          let parent_cls = 
            (match List.find_opt (fun c -> c.class_name = parent_name) p.classes with
            | Some cls -> cls
            | None -> error ("Class not found: " ^ cls.class_name)) in
          collect_attributes parent_cls
      | None -> []
    in
    parent_attributes @ (List.map2 (fun (id, ty) (id, init) -> (id, ty, init)) cls.attributes cls.attr_init_vals)
  in
  let default_value_for_type typ =
    match typ with
    | TInt -> VInt 0
    | TBool -> VBool false
    | TVoid -> Null
    | TClass _ -> Null
    | TArray inner_type -> 
        (* Si c'est un tableau, on retourne une valeur par défaut du type de tableau intérieur *)
        VArray (Array.make 0 Null)  (* Tableau vide pour des tableaux imbriqués *)
  in
  (* Table globale pour stocker les attributs statiques des classes *)
  let static_fields = Hashtbl.create 16

  in
  let rec eval_expr (e :expr) env this super =
    match e.expr with
    | Int n -> VInt n
    | Bool b -> VBool b
    | Unop (Opp, e1) ->
        (match eval_expr e1 env this super with
         | VInt n -> VInt (-n)
         | _ -> error ("Unary '-' applied to non-integer (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    | Unop (Not, e1) ->
        (match eval_expr e1 env this super with
         | VBool b -> VBool (not b)
         | _ -> error ("Unary 'not' applied to non-boolean (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    |Unop(Cast(t), e1) ->  
       (match t, eval_expr e1 env this super with
       |_,VInt n -> VInt n 
       |_, VBool b -> VBool b
       |TClass a, VObj obj -> if (class_incluse p.classes obj.cls a ) then VObj obj 
                              else Typechecker.error ("Impossible typecast at line: "^string_of_int e.loc.pos_lnum^" in file: "^e.loc.pos_fname)
       |_,_ -> error("Trying to cast to a non-type (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")")
                              )
    | Binop (op, e1, e2) ->
        let v1 = eval_expr e1 env this super in
        let v2 = eval_expr e2 env this super in
        eval_binop op v1 v2
    | Get (Var x) ->
        (try Hashtbl.find env x
         with Not_found -> error ("Variable not found: " ^ x ^ " (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    | Get (Field (e, field)) ->
        (match eval_expr e env this super with
         | VObj o -> (
          try Hashtbl.find o.fields field
          with Not_found ->
            (* Si introuvable, rechercher dans les champs statiques *)
            let cname = o.cls in
            (match Hashtbl.find_opt static_fields cname with
            | Some class_static_fields ->
                (try 
                    Hashtbl.find class_static_fields field
                  with Not_found -> 
                    error ("Field not found: " ^ field ^" (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
            | None ->
                error ("Static fields not initialized for class: " ^ cname ^" (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")")))
         | _ -> error ("Field access on non-object (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    | This -> (match this with
               | Some obj -> obj
               | None -> error ("Unbound 'this' in the current context (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    | Super -> (match super with
                | Some obj -> obj
                | None -> error ("Unbound 'super' in the current context (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    | New cname -> create_object cname
    | NewCstr (cname, args) ->
        let obj = create_object cname in
        (match obj with
        | VObj obj -> call_method obj "constructor" args env this super
        | _ -> error ("New object creation failed (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    | MethCall (obj_expr, mname, args) ->
        (match eval_expr obj_expr env this super with
         | VObj obj -> call_method obj mname args env this super
         | _ -> error ("Method call on non-object (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    | InstanceOf (e, cname) -> 
        (match eval_expr e env this super with
         | VObj obj -> VBool (class_incluse p.classes obj.cls cname)
         | _ -> error ("Instanceof on non-object (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))
    | EArrayCreate (typ, dims) -> 
      (match dims with
      | [] -> error ("Array dimensions cannot be empty (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")")
      | _ ->
          (* Évalue les dimensions *)
          let dim_sizes = List.map (fun dim_expr ->
            match eval_expr dim_expr env this super with
            | VInt size when size > 0 -> size
            | VInt size when size <= 0 -> error ("Array size must be positive (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")")
            | _ -> error ("Array dimensions must be integers(line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")")
          ) dims in
                
          (* Fonction récursive pour créer un tableau multidimensionnel *)
          let rec create_nested_array dims  =
            match dims with
            | [] -> failwith "Dimensions list cannot be empty"
            | [dim] -> (
                match typ with
                | TInt -> VArray (Array.init dim (fun _ -> VInt 0))
                | TBool -> VArray (Array.init dim (fun _ -> VBool false))
                | TVoid -> VArray (Array.init dim (fun _ -> Null))
                | TClass _ -> VArray (Array.init dim (fun _ -> Null))
                | _ -> error ("Can't handle this data type")
                )
            | dim :: rest ->
                VArray (Array.init dim (fun _ -> create_nested_array rest ))
          in

          (* Crée le tableau avec les tailles données *)
          let array_value = create_nested_array dim_sizes in

          (* Retourner la valeur du tableau créé *)
          array_value)
    | Get (ArrayAccess (array_name, indices)) ->
      (try
        let array_val = Hashtbl.find env array_name in
        let indices_values = List.map (fun idx_expr ->
          match eval_expr idx_expr env this super with
          | VInt idx when idx >= 0 -> idx
          | VInt idx when idx < 0 -> error ("Array index must be non-negative (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")")
          | _ -> error ("Array indices must be integers (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")")
        ) indices in
        let rec access_nested_array arr dims =
          match arr, dims with
          | VArray nested_array, idx :: rest when idx < Array.length nested_array ->
              if rest = [] then nested_array.(idx)
              else access_nested_array nested_array.(idx) rest
          | VArray _, idx :: _ -> error ("Array index out of bounds (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")")
          | _, _ -> error ("Invalid array access (line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^ ")" )
        in
        access_nested_array array_val indices_values
      with Not_found -> error ("Array not found: " ^ array_name ^"(line: " ^ string_of_int e.loc.pos_lnum  ^" of: " ^ e.loc.pos_fname^")"))

  (* Fonction de création d'un objet d'instance *)
  and create_object cname =
    match List.find_opt (fun c -> c.class_name = cname) p.classes with
    | Some cls ->
        (* Initialisation des attributs statiques si ce n'est pas encore fait *)
        init_static_fields cls;
        (* Création des attributs d'instance *)
        let fields = Hashtbl.create 16 in
        List.iter 
          (fun (field, ty, opt_init) ->
            if (List.exists (fun (ffield, test) -> (ffield = field) && (test) && (opt_init == None)) cls.is_attr_final)
              then Hashtbl.add fields field Null;
            if not (List.exists (fun (sfield, test) -> (sfield = field) && (test)) cls.static_attribut) 
              && not (List.exists (fun (ffield, test) -> (ffield = field) && (test) && (opt_init == None)) cls.is_attr_final)
            then (match opt_init with
            | Some expr -> Hashtbl.add fields field (eval_expr expr env None None)
            | None ->  Hashtbl.add fields field (default_value_for_type ty))
          )
          (collect_attributes cls);

        VObj { cls = cname; fields = fields }
    | None -> error ("Class not found: " ^ cname)
    
  (* Initialiser les attributs statiques d'une classe *)
  and init_static_fields cls =
    match Hashtbl.find_opt static_fields cls.class_name with
    | None -> 
        (* Initialise les valeurs des champs statiques *)
        let fields = Hashtbl.create 16 in
        List.iter2 (fun (field, is_static) (field, opt_expr) -> 
            if is_static then Hashtbl.add fields field (
              match opt_expr with
              | Some expr -> eval_expr expr env None None
              | None -> default_value_for_type (List.assoc field cls.attributes)
            )) cls.static_attribut cls.attr_init_vals;
        Hashtbl.add static_fields cls.class_name fields
    | Some _ -> () (* Ne rien faire si déjà initialisé *)

  and class_incluse classes cname1 cname2 =
    if cname1 = cname2 then true
    else
      let cls = find_class cname1 classes in
      match cls.parent with
      | Some parent -> class_incluse classes parent cname2
      | None -> false

  and eval_binop op v1 v2 =
    match op, v1, v2 with
    | Add, VInt n1, VInt n2 -> VInt (n1 + n2)
    | Sub, VInt n1, VInt n2 -> VInt (n1 - n2)
    | Mul, VInt n1, VInt n2 -> VInt (n1 * n2)
    | Div, VInt n1, VInt n2 -> VInt (n1 / n2)
    | Rem, VInt n1, VInt n2 -> VInt (n1 mod n2)
    | Lt, VInt n1, VInt n2 -> VBool (n1 < n2)
    | Le, VInt n1, VInt n2 -> VBool (n1 <= n2)
    | Gt, VInt n1, VInt n2 -> VBool (n1 > n2)
    | Ge, VInt n1, VInt n2 -> VBool (n1 >= n2)
    | Eq, v1, v2 -> VBool (v1 = v2)
    | Neq, v1, v2 -> VBool (v1 <> v2)
    | And, VBool b1, VBool b2 -> VBool (b1 && b2)
    | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
    | Structeg, v1, v2 -> VBool (structural_eq v1 v2)
    | Structineg, v1, v2 -> VBool (not (structural_eq v1 v2))
    | _ -> error "Invalid binary operation or operand types"

  and structural_eq v1 v2 =
    match v1, v2 with
    | VInt n1, VInt n2 -> n1 = n2
    | VBool b1, VBool b2 -> b1 = b2
    | VObj o1, VObj o2 ->
        o1.cls = o2.cls &&
        (* Comparer les champs des deux objets *)
        Hashtbl.fold (fun field_name field_value acc ->
          acc && 
          (try structural_eq field_value (Hashtbl.find o2.fields field_name)
            with Not_found -> false)
        ) o1.fields true
    | VArray arr1, VArray arr2 ->
        Array.length arr1 = Array.length arr2 &&
        Array.for_all2 structural_eq arr1 arr2
    | Null, Null -> true
    | _, _ -> false

  and call_method obj mname args env this super =
    let cls = find_class obj.cls p.classes in
    let method_ =
      match List.find_opt (fun m -> m.method_name = mname) cls.methods with
      | Some m -> m
      | None -> error ("Method not found: " ^ mname)
    in
    let arg_values = List.map (fun a -> eval_expr a env this super) args in
    let local_env = add_params_to_env method_.params arg_values env in
    let local_envv = add_params_to_env method_.locals (List.map (fun (ident, opt_expr) ->
      match opt_expr with
      | Some expr -> eval_expr expr env this super
      | None -> default_value_for_type (List.assoc ident method_.locals)
    ) method_.locals_init_vals) local_env in
    try
      let parent = match cls.parent with Some parent -> parent | None -> "void" in
      exec_seq method_.code local_envv (Some (VObj obj)) (Some (VObj { cls = parent; fields = obj.fields }));
      if String.equal mname "constructor" then VObj obj
      else Null
    with Return v -> v

  and exec_seq seq env this super =
    try List.iter (fun instr -> exec_instr instr env this super) seq
    with Return v -> raise (Return v)
  
  and find_field_is_final cls field =
    try List.assoc field cls.is_attr_final
    with Not_found ->
      match cls.parent with
      | Some parent_name ->
          let parent_cls = find_class parent_name p.classes in
          find_field_is_final parent_cls field
      | None -> error ("Field not found: " ^ field)
  
  and exec_instr i env this super =
    match i with
    | Print e ->
      (match eval_expr e env this super with
      | VInt n -> Printf.printf "%d\n" n
      | VBool b -> Printf.printf "%b\n" b
      | VObj _ -> Printf.printf "<object>\n"
      | VArray arr ->
      (* Affiche les éléments du tableau statique *)
      Printf.printf "[";
      Array.iteri (fun i value ->
        (match value with
          | VInt n -> Printf.printf "%d" n
          | VBool b -> Printf.printf "%b" b
          | VObj _ -> Printf.printf "<object>"
          | VArray _ -> Printf.printf "<nested array>"
          | Null -> Printf.printf "null");
        if i < Array.length arr - 1 then Printf.printf ", ";
      ) arr;
      Printf.printf "]\n";
      | Null -> Printf.printf "null\n")
    | Set (Var x, e) ->
        let v = eval_expr e env this super in
        Hashtbl.replace env x v
    | Set (Field (obj_expr, field), e) ->
        let v = eval_expr e env this super in
        (match eval_expr obj_expr env this super with
        | VObj obj -> 
          let cname = obj.cls in
          let cls = find_class cname p.classes in
          let is_final = find_field_is_final cls field in
          let is_static = List.exists (fun (sfield, test) -> (sfield = field) && (test)) cls.static_attribut in
          if not is_static then (
            (* Mise à jour du champ d'instance *)
            let vfield = 
              (try Hashtbl.find obj.fields field
                with Not_found -> 
                  try Hashtbl.find env field
                  with Not_found -> error ("Field not found: " ^ field))
            in
            if is_final && vfield <> Null then error ("Field is final: " ^ field);
            (* Si field est final et initialisé alors on plante sinon on continue *)
            Hashtbl.replace obj.fields field v
          )
          else (
            (* Mise à jour du champ statique *)
            match Hashtbl.find_opt static_fields cname with
            | Some class_static_fields ->
              let vfield = 
                (try Hashtbl.find class_static_fields field
                  with Not_found -> 
                    try Hashtbl.find env field
                    with Not_found -> error ("Field not found: " ^ field))
              in
              if is_final && vfield <> Null then error ("Field is final: " ^ field);
              (* Si field est final et initialisé alors on plante sinon on continue *)
              Hashtbl.replace class_static_fields field v
            | None ->
              (* Impossible, champs statiques non initialisés *)
              error ("Static fields not initialized for class: " ^ cname)
          )
        | _ -> error "Field assignment on non-object")
    | Set (ArrayAccess (array_name, indices), e) ->
      (* Evaluate all index expressions *)
      let index_vals = List.map (fun index_expr -> 
        match eval_expr index_expr env this super with
        | VInt idx -> idx
        | _ -> error "Array indices must be integers"
      ) indices in
      
      (* Retrieve the array value from the environment *)
      (match Hashtbl.find_opt env array_name with
      | Some (VArray arr) ->
          let rec set_value_in_array nested_array index_list new_value =
            match index_list with
            | [last_idx] -> 
                if last_idx >= 0 && last_idx < Array.length nested_array then
                  nested_array.(last_idx) <- new_value
                else error "Array index out of bounds"
            | idx :: rest ->
                if idx >= 0 && idx < Array.length nested_array then
                  (match nested_array.(idx) with
                  | VArray sub_array -> set_value_in_array sub_array rest new_value
                  | _ -> error "Invalid array structure during access")
                else error "Array index out of bounds"
            | [] -> error "Index list cannot be empty"
          in
          
          (* Evaluate the new value and assign it to the array *)
          let new_value = eval_expr e env this super in
          set_value_in_array arr index_vals new_value
      | _ -> error "Array not found or invalid structure for assignment")
    | If (cond, then_seq, else_seq) ->
        (match eval_expr cond env this super with
         | VBool true -> exec_seq then_seq env this super
         | VBool false -> exec_seq else_seq env this super
         | _ -> error "If condition must be a boolean")
    | UIf (cond, then_seq) ->
        (match eval_expr cond env this super with
         | VBool true -> exec_seq then_seq env this super
         | VBool false -> ignore()
         | _ -> error "If condition must be a boolean")
    | While (cond, body) ->
        let rec loop () =
          match eval_expr cond env this super with
          | VBool true -> exec_seq body env this super; loop ()
          | VBool false -> ()
          | _ -> error "While condition must be a boolean"
        in
        loop ()
    | Return e -> raise (Return (eval_expr e env this super))
    | Expr e -> ignore (eval_expr e env this super)

  and find_class cname classes =
    match List.find_opt (fun c -> c.class_name = cname) classes with
    | Some cls -> cls
    | None -> error ("Class not found: " ^ cname)

  and add_params_to_env params args env =
    let local_env = Hashtbl.copy env in
    List.iter2 (fun (name, _) arg -> Hashtbl.add local_env name arg) params args;
    local_env

  in
  (* Initialize global variables *)
  List.iter (fun (x, opt) ->
    match opt with
    | Some v -> Hashtbl.add env x v
    | None -> Hashtbl.add env x (default_value_for_type (List.assoc x p.globals))
  ) (List.map (fun (ident, opt_expr) ->
    match opt_expr with
    | Some expr -> (ident, Some (eval_expr expr env None None))
    | None -> (ident, None)
    ) p.globals_init_vals);
  exec_seq p.main env None None
