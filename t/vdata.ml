open Hdf
open Bigarray

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
