module G :
  sig
    type ('a, 'b, 'c) t = ('a, 'b, 'c) Bigarray.Genarray.t
    external create :
      ('a, 'b) Bigarray.kind ->
      'c Bigarray.layout -> int array -> ('a, 'b, 'c) t = "caml_ba_create"
    external num_dims : ('a, 'b, 'c) t -> int = "caml_ba_num_dims"
    val dims : ('a, 'b, 'c) t -> int array
    external nth_dim : ('a, 'b, 'c) t -> int -> int = "caml_ba_dim"
    external kind : ('a, 'b, 'c) t -> ('a, 'b) Bigarray.kind = "caml_ba_kind"
    external layout : ('a, 'b, 'c) t -> 'c Bigarray.layout = "caml_ba_layout"
    external get : ('a, 'b, 'c) t -> int array -> 'a = "caml_ba_get_generic"
    external set : ('a, 'b, 'c) t -> int array -> 'a -> unit
      = "caml_ba_set_generic"
    external sub_left :
      ('a, 'b, Bigarray.c_layout) t ->
      int -> int -> ('a, 'b, Bigarray.c_layout) t = "caml_ba_sub"
    external sub_right :
      ('a, 'b, Bigarray.fortran_layout) t ->
      int -> int -> ('a, 'b, Bigarray.fortran_layout) t = "caml_ba_sub"
    external slice_left :
      ('a, 'b, Bigarray.c_layout) t ->
      int array -> ('a, 'b, Bigarray.c_layout) t = "caml_ba_slice"
    external slice_right :
      ('a, 'b, Bigarray.fortran_layout) t ->
      int array -> ('a, 'b, Bigarray.fortran_layout) t = "caml_ba_slice"
    external blit : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit = "caml_ba_blit"
    external fill : ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
    val map_file :
      Unix.file_descr ->
      ?pos:int64 ->
      ('a, 'b) Bigarray.kind ->
      'c Bigarray.layout -> bool -> int array -> ('a, 'b, 'c) t
    external unsafe_set :
      ('a, 'b, 'c) Bigarray.Genarray.t -> int array -> 'a -> unit
      = "unsafe_caml_ba_set_generic"
    external unsafe_get : ('a, 'b, 'c) Bigarray.Genarray.t -> int array -> 'a
      = "unsafe_caml_ba_get_generic"
    external elems : ('a, 'b, 'c) Bigarray.Genarray.t -> int = "ml_ba_elems"
    val apply_ml : ('a -> 'a) -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
    external _c_apply :
      string -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit
      = "ml_ba_apply"
    val apply :
      ('a -> 'a) -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit
    val map :
      ('a -> 'b) ->
      ('b, 'c) Bigarray.kind ->
      ('a, 'd, 'e) Bigarray.Genarray.t -> ('b, 'c, 'e) Bigarray.Genarray.t
    external _c_map :
      string ->
      ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
      ('c, 'd, Bigarray.c_layout) Bigarray.Genarray.t -> unit = "ml_ba_map"
    val map_fast :
      ('a -> 'a) ->
      ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
      ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
    val fold :
      ('a -> 'b -> 'a) -> 'a -> ('b, 'c, 'd) Bigarray.Genarray.t -> 'a
    val to_array : ('a, 'b, 'c) Bigarray.Genarray.t -> 'a array
  end
type t =
    Int8 of
      (int, Bigarray.int8_signed_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | UInt8 of
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
      Bigarray.Genarray.t
  | Int16 of
      (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | UInt16 of
      (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout)
      Bigarray.Genarray.t
  | Int32 of
      (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | Float32 of
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | Float64 of
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t
type data_t =
    [ `FLOAT32 | `FLOAT64 | `INT16 | `INT32 | `INT8 | `UINT16 | `UINT8 ]
exception BadDataType of string * string
external is_hdf : string -> bool = "ml_Hishdf"
val create : data_t -> int array -> t
val _string_from_data_type : data_t -> string
val _type_size_in_bytes : data_t -> int
val _data_type_from_string : string -> data_t
val _data_type_from_t : t -> data_t
val dims : t -> int array
val sub : t -> int -> int -> t
val reshape : t -> int array -> t
val slice : t -> int array -> t
val blit : t -> t -> unit
val get_int : t -> int array -> int
val get_int32 : t -> int array -> int32
val get_float : t -> int array -> float
val set_int : t -> int array -> int -> unit
val set_int32 : t -> int array -> int32 -> unit
val set_float : t -> int array -> float -> unit
val elems : t -> int
val apply_int : (int -> int) -> t -> unit
val apply_int32 : (int32 -> int32) -> t -> unit
val apply_float : (float -> float) -> t -> unit
val map_int :
  (int -> 'a) ->
  ('a, 'b) Bigarray.kind ->
  t -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
val map_int32 :
  (int32 -> 'a) ->
  ('a, 'b) Bigarray.kind ->
  t -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
val map_float :
  (float -> 'a) ->
  ('a, 'b) Bigarray.kind ->
  t -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
val fold_int : ('a -> int -> 'a) -> 'a -> t -> 'a
val fold_int32 : ('a -> int -> 'a) -> 'a -> t -> 'a
val fold_float : ('a -> float -> 'a) -> 'a -> t -> 'a
module SD :
  sig
    type interface_id = InterfaceID of int
    type data_id = DataID of int
    type data_spec = {
      index : data_id;
      name : string;
      data_type : data_t;
      dimensions : int array;
    }
    external c_open_file : string -> int = "ml_SDstart"
    val open_file : string -> interface_id
    external c_get_info :
      int -> int -> int * string * string * int array * int = "ml_SDgetinfo"
    val get_info : interface_id -> data_id -> data_spec
    external c_get_data :
      int -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> string
      = "ml_SDreaddata"
    val get_data :
      interface_id ->
      data_id -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> string
    external c_close_file : int -> int = "ml_SDend"
    val close_file : interface_id -> int
    val get_spec_list : interface_id -> data_spec list
    val get_index_from_name : interface_id -> string -> data_id
    val _get_data : interface_id -> data_id -> t -> string
    val get_data_by_name : interface_id -> string -> t
    val get_data_by_index : interface_id -> data_id -> t
    external c_open_sd_create : string -> int = "ml_SDstart_create"
    val open_sd_create : string -> interface_id
    external c_create :
      int ->
      string ->
      string -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> int
      = "ml_SDcreate"
    val create : interface_id -> string -> data_t -> int array -> data_id
    external c_write_data :
      int -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit
      = "ml_SDwritedata"
    val write_data : data_id -> t -> unit
    val create_data : interface_id -> string -> t -> unit
    external c_end_access : int -> int = "ml_SDendaccess"
    val end_access : data_id -> int
  end
module Vdata :
  sig
    type interface_id = InterfaceID of int32
    type data_id = DataID of int32
    type data_spec = {
      index : data_id;
      name : string;
      field_name_list : string;
      data_size : int;
      records : int;
    }
    external c_open_hdf : string -> int32 = "ml_Hopen"
    val open_hdf : string -> interface_id
    external c_close_hdf : int32 -> int = "ml_Hclose"
    val close_hdf : interface_id -> int
    external c_start : int32 -> int32 = "ml_Vstart"
    val start : interface_id -> data_id
    external c_get_info :
      int32 -> int32 -> int32 * string * string * int * int = "ml_VSinquire"
    val get_info : interface_id -> data_id -> data_spec
    val get_spec_list : interface_id -> data_spec list
    external c_get_data :
      int32 ->
      int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> string
      = "ml_VSread"
    val _get_data : interface_id -> data_id -> t -> string
    external c_vd_index_from_vdata_ref : int32 -> int32 -> int32
      = "ml_vd_index_from_vdata_ref"
    val vd_index_from_vdata_ref : interface_id -> data_id -> data_id
    external c_VSfind : int32 -> string -> int32 = "ml_VSfind"
    val get_index_from_name : interface_id -> string -> data_id
    val get_data_by_name : interface_id -> string -> data_t -> t
    val get_data_by_index : interface_id -> data_id -> data_t -> t
    external c_v_end : int32 -> int = "ml_Vend"
    val v_end : interface_id -> int
    external open_hdf_create : string -> int = "ml_Hopen_create"
    external c_open_hdf_rw : string -> int32 = "ml_Hopen_rw"
    val open_hdf_rw : string -> interface_id
    external c_create : int32 -> int32 = "ml_VSattach_create"
    val create : interface_id -> data_id
    external c_define_fields : int32 -> string -> string -> int -> unit
      = "ml_VSfdefine"
    val define_fields : data_id -> string -> data_t -> int -> unit
    external c_set_fields : int32 -> string -> unit = "ml_VSsetfields"
    val set_fields : data_id -> string -> unit
    external c_write :
      int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> int -> unit
      = "ml_VSwrite"
    val write : data_id -> t -> int -> unit
    external c_set_name : int32 -> string -> unit = "ml_VSsetname"
    val set_name : data_id -> string -> unit
    external c_set_class : int32 -> string -> unit = "ml_VSsetclass"
    val set_class : data_id -> string -> unit
    external c_detach : int32 -> unit = "ml_VSdetach"
    val detach : data_id -> unit
  end
