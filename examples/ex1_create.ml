open Hdf.Easy
open Batteries
module Ba = Bigarray

let () =
  (* Create some example data *)
  let matrix = Ba.Array2.create Ba.float32 Ba.c_layout 100 100 in
  for i = 0 to 99 do
    for j = 0 to 99 do
      matrix.{i, j} <- float_of_int ((i + 1) * (j + 1))
    done
  done;
  (* Wrap the data in an Hdf4.t *)
  let hdf4_matrix = Hdf4c.of_float32 (Ba.genarray_of_array2 matrix) in
  (* Create an SDS *)
  let sd_matrix = Hdf4c.Sd.make "example_data" hdf4_matrix in
  (* Save the data out to disk *)
  Hdf4c.open_file_in ~access:Hdf4c.L.DFACC_CREATE (
    fun h ->
      Hdf4c.Sd.write h sd_matrix
  ) "example.hdf4";
  ()

