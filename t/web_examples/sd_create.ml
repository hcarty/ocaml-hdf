(*******************************************************************/
/*                                                                 */
/*  sd_create.c                                                    */
/*        This program creates an HDF file, sd.hdf, which          */
/*        contains an SDS of size 10 x 5.                          */
/*                                                                 */
/*******************************************************************)

open Hdf
open Printf
open Bigarray

let dim0 = 10
let dim1 = 5

let () =
  (*
    int32 sd_id, sds_id, istat;
    int32 dims[2], start[2], edges[2], rank;
    int16 array_data[DIM0][DIM1];
    intn i, j;
  *)

  let array_data = Array2.create int16_signed c_layout dim0 dim1 in

  (* Create and open the file and initiate the SD interface. *)
  let sd_id = sd_start "sd.hdf" DFACC_CREATE in
  printf "\n... sd.hdf created\n";

  (* Define the rank and dimensions of the data set to be created. *)
  let rank = 2 in
  let dims = [| dim0; dim1 |] in

  (* Create the array data set. *)
  let sds_id = sd_create sd_id "data1" DFNT_INT16 (Array.map Int32.of_int dims) in
  if sds_id = -1l then (
    printf "SDcreate failed.\n";
    let error_id = he_value 0l in
    printf "HDF Error: %s\n" (he_string error_id);
    exit (-1);
  )
  else (
    printf "... data1 created\n";
  );

  (* Fill the array with values. *)
  for j = 0 to dim0 - 1 do
    for i = 0 to dim1 - 1 do
      array_data.{j,i} <- (i + j) + 1;
    done
  done;

  (* This is not actually supported in the OCaml wrapper... the following code
     shows the assumed start and edges values.
  (* Define the location, pattern, and size of the data set *)
  let start = Array.make rank 0l in
  let edges = Array.init rank (fun i -> Int32.of_int dims.(i)) in
  *)

  sd_writedata sds_id (genarray_of_array2 array_data);
  (*
  if istat = -1l then (
    printf "SDwritedata failed.\n";
    let error_id = he_value 0l in
    printf "HDF Error: %s\n" (he_string error_id);
    exit (-1);
  )	
  else
  *)
  printf "... data written out\n";

  (* Terminate access to the array. *)
  sd_endaccess sds_id;

  (* Terminate access to the SD interface and close the file. *)
  sd_end sd_id;
  printf("... file closed\n\n");
  ()


