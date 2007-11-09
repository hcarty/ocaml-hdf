(* This is an almost direct translation of the vd_ex3.c example program
   available at http://www.hdfgroup.org/training/HDFtraining/examples/vd_ex3.c
   or in this directory.
*)
open Hdf
open Bigarray

let field_name = "Field Entries"
let number_of_rows = 10
let order = 3l

let () =
  (* Open the HDF file. *)
  let file_id = h_open "VD_Ex3.hdf" DFACC_CREATE 0 in

  (* Initialize HDF Vgroup/Vdata interface. *)
  let _ = v_start file_id in

  (* Create a new Vdata. *)
  let vdata_id = vs_attach file_id (-1l) "w" in

  (* Define the field data name, type and order. *)
  let _ = vs_fdefine vdata_id field_name DFNT_INT16 order in

  (* Specify the field(s) that will be written to. *)
  let _ = vs_setfields vdata_id field_name in

  (* Generate the Vset data. *)
  let vdata_buf =
    genarray_of_array1 (
      Array1.of_array int16_signed c_layout (
        Array.init (number_of_rows * (Int32.to_int order)) (fun x -> x)
      )
    )
  in

  (* Write the data to the Vset. *)
  let _ = vs_write vdata_id vdata_buf number_of_rows in

  (* Set the name and class. *)
  let _ = vs_setname vdata_id "Example Vdata" in
  let _ = vs_setclass vdata_id "Example Vdata" in

  (* Terminate access to the Vdata. *)
  let _ = vs_detach vdata_id in

  (* Terminate access to the Vset interface and close the file. *)
  let _ = v_end file_id in
  let _ = h_close file_id in
  ()
