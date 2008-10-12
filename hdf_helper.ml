open Hdf
open Bigarray

(** Experimental Vgroup section *)

(** Example:
Vgroup [|
  Vgroup [|
    SDSData (Hdf4.create `int8 [|10|])
  |];
  SDSData (Hdf4.create `int8 [|10|]);
|]
*)
type group_elt =
  | Vgroup of group_elt array
  | SDSData of Hdf4.t

(** Safe and high level SDS handling functions. *)

type ('a, 'b) sds_kind_t = {
  read_data : ?name:string -> ?index:int32 -> Hdf4.interface ->
    ('a, 'b, c_layout) Genarray.t;
  write_data : ('a, 'b, c_layout) Genarray.t -> int32 -> unit;
  read_fill : int32 -> 'a;
  write_fill : int32 -> 'a -> unit;
  data_type : Hdf4.data_t;
}

let sd_read_data data_type =
  fun ?name ?index interface ->
    SD.read_ga ?name ?index data_type interface

let sd_write_data d sds_id = SD.write_data sds_id d

let hdf_float32 = {
  read_fill = sd_getfillvalue_float;
  write_fill = sd_setfillvalue_float;
  read_data = sd_read_data float32;
  write_data = (fun d -> sd_write_data (Hdf4.Float32 d));
  data_type = `float32;
}

let hdf_float64 = {
  read_fill = sd_getfillvalue_float;
  write_fill = sd_setfillvalue_float;
  read_data = sd_read_data float64;
  write_data = (fun d -> sd_write_data (Hdf4.Float64 d));
  data_type = `float64;
}

let hdf_int8 = {
  read_fill = sd_getfillvalue_int;
  write_fill = sd_setfillvalue_int;
  read_data = sd_read_data int8_signed;
  write_data = (fun d -> sd_write_data (Hdf4.Int8 d));
  data_type = `int8;
}

let hdf_uint8 = {
  read_fill = sd_getfillvalue_int;
  write_fill = sd_setfillvalue_int;
  read_data = sd_read_data int8_unsigned;
  write_data = (fun d -> sd_write_data (Hdf4.UInt8 d));
  data_type = `uint8;
}

let hdf_int16 = {
  read_fill = sd_getfillvalue_int;
  write_fill = sd_setfillvalue_int;
  read_data = sd_read_data int16_signed;
  write_data = (fun d -> sd_write_data (Hdf4.Int16 d));
  data_type = `int16;
}

let hdf_uint16 = {
  read_fill = sd_getfillvalue_int;
  write_fill = sd_setfillvalue_int;
  read_data = sd_read_data int16_unsigned;
  write_data = (fun d -> sd_write_data (Hdf4.UInt16 d));
  data_type = `uint16;
}

let hdf_int32 = {
  read_fill = sd_getfillvalue_int32;
  write_fill = sd_setfillvalue_int32;
  read_data = sd_read_data int32;
  write_data = (fun d -> sd_write_data (Hdf4.Int32 d));
  data_type = `int32;
}

module Attribute = struct
  type t = {
    name : string;
    data : Hdf4.t;
  }
end

let wrap_sds_call f ?name ?index interface =
  try_finally
    (SD.select ?name ?index interface)
    sd_endaccess
    f

(*
type fill_value_t =
  | IntFill of int
  | FloatFill of float
  | Int32Fill of int32

let read_fill ?name ?index interface =
  let f g = wrap_sds_call g ?name ?index interface in
  function
    | Hdf4.Int8 _
    | Hdf4.UInt8 _
    | Hdf4.Int16 _
    | Hdf4.UInt16 _ -> IntFill (f sd_getfillvalue_int)
    | Hdf4.Int32 _ -> Int32Fill (f sd_getfillvalue_int32)
    | Hdf4.Float32 _
    | Hdf4.Float64 _ -> FloatFill (f sd_getfillvalue_float)
*)

type ('a, 'b) sds_data_t = {
  (* The SDS entry name *)
  name : string;
  (* The actual SDS contents *)
  data : ('a, 'b, c_layout) Genarray.t;
  (* Attributes associated with this SDS *)
  attrs : Attribute.t array;
  (* Fill value for missing or unset values *)
  fill : 'a option;
  (* What kind of data are these? *)
  kind : ('a, 'b) sds_kind_t;
}

let read_sds_attributes sds_id =
  let (sds_name, dims, data_type, num_attrs) = SD.info_sds sds_id in
  Array.init num_attrs (
    fun attr_index ->
      let attr_index = Int32.of_int attr_index in
      (* Find out the attribute information *)
      let (name, data_type, count) = sd_attrinfo sds_id attr_index in
      let count = Int32.to_int count in
      (* Allocate space for, then read, the attribute data *)
      let data =
        Hdf4.create (Hdf4.hdf_datatype_to_mlvariant data_type) [|count|]
      in
      let f x = sd_readattr sds_id attr_index x in
      {
        Attribute.name = name;
        data =
          let () =
            match data with
            | Hdf4.Int8 x -> f x
            | Hdf4.UInt8 x -> f x
            | Hdf4.Int16 x -> f x
            | Hdf4.UInt16 x -> f x
            | Hdf4.Int32 x -> f x
            | Hdf4.Float32 x -> f x
            | Hdf4.Float64 x -> f x
          in
          data
      }
  )

let write_sds_attributes attrs sds_id =
  (*let f' name t count x = sd_setattr sds_id name t count x in*)
  Array.iter (
    fun attr ->
      let f x =
        sd_setattr sds_id attr.Attribute.name
          (Hdf4.mlvariant_to_hdf_datatype
            (Hdf4._data_type_from_t attr.Attribute.data))
          (Int32.of_int (Hdf4.dims attr.Attribute.data).(0))
          x
      in
      match attr.Attribute.data with
      | Hdf4.Int8 x -> f x
      | Hdf4.UInt8 x -> f x
      | Hdf4.Int16 x -> f x
      | Hdf4.UInt16 x -> f x
      | Hdf4.Int32 x -> f x
      | Hdf4.Float32 x -> f x
      | Hdf4.Float64 x -> f x
  ) attrs

let unless except f x =
  try
    Some (f x)
  with
  | e when e = except -> None

let read_sd ?name ?index kind interface =
  let (sds_name, dims, data_type, num_attrs) = SD.info ?name ?index interface in
  (* Make sure the data type matches.  Otherwise bomb out. *)
  if data_type <> kind.data_type then
    raise (Invalid_argument "SDS data type mismatch")
  else
    {
      name = sds_name;
      data = kind.read_data ?name ?index interface;
      attrs = wrap_sds_call read_sds_attributes ?name ?index interface;
      fill =
        unless (Failure "Error getting SDS fill value.")
          (fun f -> wrap_sds_call f ?name ?index interface)
          kind.read_fill;
      kind = kind;
    }

let create_sd data interface =
  match
    sd_create interface.Hdf4.sdid data.name
      (Hdf4.mlvariant_to_hdf_datatype data.kind.data_type)
      (Array.map Int32.of_int (Genarray.dims data.data))
  with
  | (-1l) ->
      let error_str = he_string (he_value 0l) in
      raise (Hdf4.HdfError error_str)
  | sds_id -> sds_id

let write_sd data interface =
  try_finally
    (create_sd data interface)
    sd_endaccess
    (
      fun sds_id ->
        (* Write the data, attributes and fill value (if there is one). *)
        data.kind.write_data data.data sds_id;
        write_sds_attributes data.attrs sds_id;
        Option.may (data.kind.write_fill sds_id) data.fill;
    )

