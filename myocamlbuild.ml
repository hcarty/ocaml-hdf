open Ocamlbuild_plugin
open Command

let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read

let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let hdf4_linkflags cclib =
  let tag = if cclib then A "-cclib" else N in
  let all_elements =
    blank_sep_strings &
      Lexing.from_string &
      run_and_read "h4cc -show"
  in
  let is_linker_flag s = s.[0] = '-' && (s.[1] = 'l' || s.[1] = 'L') in
  let flags = List.filter is_linker_flag all_elements in
  S (List.map (fun flag -> S [tag; A flag]) flags)

(* A simple function to check if a string is empty/whitespace *)
let is_all_whitespace s =
  try
    String.iter (fun c -> if c <> ' ' then raise Exit) s;
    true
  with Exit -> false

let get_env_elem s =
  let it = getenv s in
  match is_all_whitespace it with
      true -> N
    | false -> A it

let camlidl_lib = get_env_elem "CAMLIDL_LIB"
let camlidl_lib_dir = get_env_elem "CAMLIDL_LIB_DIR"
let other_cflags = get_env_elem "CFLAGS"

(* ocamlfind packages required for compilation *)
let packages = "batteries pcre"

(* Build an appropriate ocamlfind command line *)
let ocamlfind cmd cc =
  S (
    [A "ocamlfind"; A cmd]
    @ (match cc with None -> [N] | Some x -> [A "-cc"; A x])
    @ [A "-package"; A packages]
  )

(* Link packages in when needed. *)
let () = flag ["ocaml"; "link"; "program"] (A "-linkpkg")

(* The camlidl command to use (default: first one found in $PATH) *)
let camlidl = S([A"camlidl"; A"-header"])

(* Files included in the main idl descriptor *)
let idl_includes =
  ["hdf_h.inc"; "mfhdf_h.inc"; "hlimits_h.idl"; "hdf_extras.idl"]
;;

dispatch begin function
  | After_options ->
      (* Redefine ocaml{c,opt,dep} to use the ocamlfind equivalents *)
      Options.ocamlc := ocamlfind "c" (Some "h4cc");
      Options.ocamlopt := ocamlfind "opt" (Some "h4cc");
      Options.ocamldep := ocamlfind "dep" None;
  | After_rules ->
      (* Handle *.idl files properly... I think *)
      rule "camlidl processing"
        ~prods:["%.mli"; "%.ml"; "%_stubs.c"]
        ~deps:["%.idl"]
        begin fun env _build ->
          let idl = env "%.idl" in
          let tags = tags_of_pathname idl++"compile"++"camlidl" in
          let cmd = Cmd(S[camlidl; T tags; P idl]) in
          Seq [cmd]
        end;

      (* gcc needs to know where to find the needed #includes *)
      flag ["c"; "compile"]
        (S[A"-ccopt"; other_cflags]);

      (* Include the HDF and camlidl compiler options for ocamlmklib *)
      flag ["ocamlmklib"; "c"]
        (S[camlidl_lib_dir; camlidl_lib; hdf4_linkflags false]);

      (* Custom tag for OCaml bytecode *)
      flag ["ocaml"; "link"; "byte"]
        (A"-custom");

      (* Use the proper extras when compiling the OCaml library *)
      flag ["ocaml"; "link"; "library"; "byte"]
        (S[A"-dllib"; A "-lhdf_stubs"; A"-cclib"; A"-lhdf_stubs";
           A"-cclib"; camlidl_lib; hdf4_linkflags true]);

      flag ["ocaml"; "link"; "library"; "native"]
        (S[A"-cclib"; A"-lhdf_stubs"; A"-cclib"; camlidl_lib;
           hdf4_linkflags true]);

      (* Make sure the C pieces and built... *)
      dep ["ocaml"; "compile"] ["libhdf_stubs.a"];

      (* Any camlidl work must be updated if the camlidl inputs are changed *)
      dep ["compile"; "camlidl"] idl_includes;

  | _ -> ()
end
