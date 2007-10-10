#!./hdf.top
#directory "/home/hcarty/Applications/ocaml-libs/";;
#load "testSimple.cma";;
#use "topfind";;
#require "bigarray";;

let a = [|1.;2.;3.;4.;5.;6.|]
let arr =
  Bigarray.genarray_of_array1
    (Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout a)
let b =
  [|
    [|1; 2; 3|];
    [|4; 5; 6|]
  |]
let arr2 =
  Hdf.Float32 (
    Bigarray.genarray_of_array2
      (Bigarray.Array2.of_arrays Bigarray.int32 Bigarray.c_layout b)
  )
let data_name = "Test Data"
let data_type = `FLOAT32
let filename = "test.hdf"

let create_file () =
  let data = Hdf.Float32 arr in

  let sd_id = Hdf.SD.open_sd_create filename in
  let sds_id =
    Hdf.SD.create sd_id data_name data_type (Bigarray.Genarray.dims arr)
  in
  let _ = Hdf.SD.write_data sds_id data in
  let _ = Hdf.SD.end_access sds_id in
  let _ = Hdf.SD.close_file sd_id in
  filename

let create_file_2 () =
  let data = Hdf.Float32 arr in

  let sd_id = Hdf.SD.open_sd_create filename in
  let _ = Hdf.SD.create_data sd_id data_name data in
  let _ = Hdf.SD.close_file sd_id in
  filename

let load_file filename =
  let sd_id = Hdf.SD.open_file filename in
  let data = Hdf.SD.get_data_by_name sd_id data_name in
  let b = Array.map (fun i -> Hdf.get_float data [|int_of_float i - 1|]) a in
  let _ = Hdf.SD.close_file sd_id in
  b


(* The tests. *)
open TestSimple

let _ =
  plan 5;
  is (Hdf.is_hdf "fake_file") false "Know a non-HDF4 file";
  is (Hdf.is_hdf (create_file ())) true "Generate valid SDS HDF4 file";
  is (load_file filename) a "Reading generated HDF4 SDS data";
  is (Hdf.is_hdf (create_file_2 ())) true "Generate valid SDS HDF4 file (simple)";
  is (load_file filename) a "Reading generated HDF4 SDS data (simple)";
  ()

(*
let field_name = "VDTEST";;
let file_id = Hdf.open_hdf_rw filename;;
let status = Hdf.v_start file_id;;
let vdata_id = Hdf.create_vs file_id;;
let aa = Hdf.define_vs_fields vdata_id field_name "FLOAT32" 1;;
let ab = Hdf.set_vs_fields vdata_id field_name;;
let ac = Hdf.write_Vdata vdata_id brr (Array.length b);;
let ad = Hdf.set_vs_name vdata_id field_name;;
let ae = Hdf.set_vs_class vdata_id field_name;;
let af = Hdf.vs_detach vdata_id;;
Hdf.v_end file_id;;
Hdf.close_hdf file_id;;
*)
