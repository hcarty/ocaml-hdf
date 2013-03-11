(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin;;
open Command;;

(* The HDF4 compiler script (a wrapper around gcc, generally) *)
let h4cc =
  BaseEnvLight.var_get "h4cc" (
    BaseEnvLight.load
      ~allow_empty:true ~filename:MyOCamlbuildBase.env_filename ()
  )
;;

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read;;
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings;;

(* Link flags appropriate for programs using HDF4, taken from h4cc *)
let hdf4_linkflags cclib =
  let tag = if cclib then A "-cclib" else N in
  let all_elements =
    blank_sep_strings (
      Lexing.from_string (
        run_and_read (h4cc ^ " -show")
      )
    )
  in
  let is_linker_flag s =
    (* Quite a dirty hack... *)
    let length = String.length s in
    length > 2 && (
      (s.[0] = '-' && (s.[1] = 'l' || s.[1] = 'L')) ||
      (s.[length - 1] = 'a' && s.[length - 2] = '.') ||
      (s.[length - 1] = 'o' && s.[length - 2] = 's' && s.[length - 3] = '.')
    )
  in
  let flags = List.filter is_linker_flag all_elements in
  S (List.map (fun flag -> S [tag; A flag]) flags)
;;

let hdf4_dispatch = function
  | After_options ->
      (* Use h4cc for C compilation *)
      Options.ocamlc := S [!Options.ocamlc; A "-cc"; A h4cc];
  | After_rules ->
      (* Keeping the camlidl rule around temporarily in case it's useful later
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
      *)

      (* PIC for 64bit systems *)
      flag ["c"; "compile"] (S [A "-ccopt"; A "-fPIC"]);

      (* Include HDF4 linking options for ocamlmklib *)
      flag ["ocamlmklib"; "c"] (hdf4_linkflags false);

      (* Use the proper extras when compiling the OCaml library *)
      flag ["ocaml"; "link"; "library"; "byte"] (hdf4_linkflags true);
      flag ["ocaml"; "link"; "library"; "native"] (hdf4_linkflags true);
  | _ -> ()
;;

dispatch (
  MyOCamlbuildBase.dispatch_combine [
    dispatch_default;
    hdf4_dispatch;
  ]
)
;;
