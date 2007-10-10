module Id = struct
  let name = "pa_hdf"
  let version = "$Id:$"
end

module HdfGet (Syntax : Camlp4.Sig.Camlp4Syntax) = struct
 include Syntax

 open Camlp4.Sig

 let concat_exprs _loc = function
     [] -> failwith "concat_exprs"
   | e::es -> List.fold_left (fun e e' -> <:expr< $e$ ^ ";" ^ $e'$ >>) e es

 let get _loc t a l =
   let m = "Hdf" in
   let f = "get_" ^ t in
   let v = <:expr< $uid:m$ . $lid:f$ >> in
   let ll = concat_exprs _loc l in
   <:expr< $v$ $a$ [| $ll$ |] >>

 let set _loc t a l new_val =
   let m = "Hdf" in
   let f = "set_" ^ t in
   let v = <:expr< $uid:m$ . $lid:f$ >> in
   <:expr< $v$ $a$ $l$ $new_val$ >>

 EXTEND Gram
 expr: LEVEL "top"
   [[ a = SELF; "{|"; t = LIDENT; "|"; l = LIST1 expr SEP ";"; "}" ->
        get _loc t a l
    | a = SELF; "{|"; t = LIDENT; "|"; l = SELF; "}" ; "<-";  v = expr ->
        set _loc t a l v
    ]];
 END
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(HdfGet)
