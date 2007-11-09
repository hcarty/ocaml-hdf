open Hdf
open Bigarray

type vdata_t = {
  n_records : int32;
  interlace : int32;
  fields : string;
  vdata_size : int32;
  vdata_name : string;
  vdata_class : string;
  num_attrs : int;
  num_fields : int;
  field_orders : int32 array;
  field_types : hdf_data_type array;
  field_sizes : int32 array;
  field_num_attrs : int array;
  data : (int, int8_unsigned_elt, c_layout) Genarray.t;
}

(*
let create_file filename =
  let file_id = h_open filename DFACC_CREATE 100 in
  v_start file_id;

  let vdata_id = vs_attach file_id (-1l) "w" in
  vs_fdefine vdata_id field_name DFNT_INT16 order;
  vs_setfields vdata_id field_name;

  let vdata_buf = Genarray.create 
*)

let prep_read () =
  let file = "2A23trmm_tmp.hdf" in
  let file_id = h_open file DFACC_READ 0 in
  v_start file_id;
  file_id

let prep_write () =
  let file = "out.hdf" in
  let file_id = h_open file DFACC_WRITE 0 in
  v_start file_id;
  file_id

let allocate_vdata_bigarray kind bytes =
  Bigarray.Genarray.create kind Bigarray.c_layout [|bytes|]

let read_vdata_with_specs vdata_id =
  let (istat, n_records, interlace, fields, vdata_size, vdata_name) =
    vs_inquire vdata_id
  in
  let (_, vdata_class) = vs_getclass vdata_id in
  let num_attrs = vs_fnattrs vdata_id (-1l) in
  let num_fields = Int32.to_int (vf_nfields vdata_id) in
  let field_orders =
    Array.init num_fields (fun i -> vf_fieldorder vdata_id (Int32.of_int i))
  in
  let field_types =
    Array.init num_fields (fun i -> vf_fieldtype vdata_id (Int32.of_int i))
  in
  let field_sizes =
    Array.init num_fields (fun i -> vf_fieldisize vdata_id (Int32.of_int i))
  in
  let field_num_attrs =
    Array.init num_fields (fun i -> vs_fnattrs vdata_id (Int32.of_int i))
  in
  (* Create a Bigarray to hold the data, and suck it all up.
     The total size in bytes of the Vdata is vdata_size * n_records.
  *)
  let data =
    allocate_vdata_bigarray int8_unsigned (Int32.to_int vdata_size * Int32.to_int n_records)
  in
  vs_setfields vdata_id fields;
  vs_read vdata_id data n_records;
  {
    n_records = n_records;
    interlace = interlace;
    fields = fields;
    vdata_size = vdata_size;
    vdata_name = vdata_name;
    vdata_class = vdata_class;
    num_attrs = num_attrs;
    num_fields = num_fields;
    field_orders = field_orders;
    field_types = field_types;
    field_sizes = field_sizes;
    field_num_attrs = field_num_attrs;
    data = data;
  }

let vdata_map f file_id =
  let vdata_ref_0 = vs_getid file_id (-1l) in
  let rec loop l vdata_ref =
    let vdata_id = vs_attach file_id vdata_ref "r" in
    if vdata_id = -1l then
      List.rev l
    else
      loop ((f vdata_id) :: l) (vs_getid file_id vdata_ref)
  in
  loop [] vdata_ref_0

let read_file filename =
  let file_id = h_open filename DFACC_READ 0 in
  v_start file_id;
  (* TODO - Actually read the file... *)
  v_end file_id;
  h_close file_id;
  ()

(*
let rec vdata_iter vdata_ref =
  let vdata_id = vs_attach file_id vdata_ref "r" in
  if vdata_id = (-1l) then
    ()
  else
    let is_attr = vs_isattr vdata_id in
    let () = Printf.printf "%d\n" is_attr in
    let (istat, n_records, interlace, fields, vdata_size, vdata_name) =
      vs_inquire vdata_id
    in
    vs_detach vdata_id;
    if is_attr = 1 then
      let () = Printf.printf "%s is an attribute\n" vdata_name in
      vdata_iter (vs_getid file_id vdata_ref)
    else
      vdata_iter (vs_getid file_id vdata_ref)
*)

let write_vdata file_id data =
  (* Vdata *)
  (*
    let file_id = h_open hdf_filename DFACC_WRITE 0 in
    v_start file_id;
  *)
  let vdata_id = vs_attach file_id (-1l) "w" in
  let field_name_array = Array.of_list (Pcre.split ~pat:"," data.fields) in
  let field_defs =
    Array.init (Array.length data.field_types)
      (fun i -> ( data.field_types.(i), field_name_array.(i), data.field_orders.(i) ))
  in
  Array.iter
    (fun (ftype, fname, forder) -> vs_fdefine vdata_id fname ftype forder)
    field_defs;

  vs_setname vdata_id data.vdata_name;
  vs_setclass vdata_id data.vdata_class;
  vs_setfields vdata_id data.fields;

  vs_write vdata_id data.data data.n_records;

  vs_detach vdata_id;
  (* Return the vdata_id so the "next" one can be created *)
  ()

let rec write_all_vdata file_id vdata_list =
  match vdata_list with
      hd :: tl ->
        print_endline ("Writing " ^ hd.vdata_name);
        write_vdata file_id hd;
        write_all_vdata file_id tl
    | [] ->
        ()
