(** {5 HDF4} *)

open Bigarray
module G = ExtBigarray

(** {6 Low Level Functions} *)

include Hdf_wrapper
(* Carry over some naming conventions from hdf.h *)
let hdf_open = h_open
let hdf_close = h_close
let v_start = v_initialize
let v_end = v_finish
(* Low-level functions wrapped by hand. *)
external vs_write: int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  int32 -> unit = "ml_VSwrite"
external vs_read: int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  int32 -> unit = "ml_VSread"

(** {6 Hight Level Functions} *)

(** The basic type which encapsulates the HDF data types we can
    support.  These are encapsulated in a variant type to avoid having
    to write explicit cases for every data type within a given HDF4
    file.
*)
type t =
    Int8 of (int, Bigarray.int8_signed_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | UInt8 of (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | Int16 of (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | UInt16 of (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | Int32 of (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | Float32 of (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Genarray.t
  | Float64 of (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t

(** A somewhat redundant type... *)
type data_t = [ `INT8 | `UINT8 | `INT16 | `UINT16 | `INT32 | `FLOAT32 | `FLOAT64 ]

(** Exception raised if we try to get at data which we shouldn't or can't handle. *)
exception BadDataType of string * string

(** {6 Internal library functions } *)

(** Check to see if a file is a valid HDF4 data file.
    [is_hdf filename] give true if [filename] is a HDF4 file, false if not.
    TODO - Remove this, it's in the lowlevel routines.
*)
external is_hdf: string -> bool = "ml_Hishdf"

(** Create a bigarray of the appropriate type, wrapped as an [Hdf.t]. *)
let create (data_type : data_t) dimensions =
  let f x = Bigarray.Genarray.create x Bigarray.c_layout dimensions in
  match data_type with
      `INT8 -> Int8 (f Bigarray.int8_signed)
    | `UINT8 -> UInt8 (f Bigarray.int8_unsigned)
    | `INT16 -> Int16 (f Bigarray.int16_signed)
    | `UINT16 -> UInt16 (f Bigarray.int16_unsigned)
    | `INT32 -> Int32 (f Bigarray.int32)
    | `FLOAT32 -> Float32 (f Bigarray.float32)
    | `FLOAT64 -> Float64 (f Bigarray.float64)

(** Get a string representation for a given data type. *)
let ( _string_from_data_type : data_t -> string ) = function
    `INT8 -> "INT8"
  | `UINT8 -> "UINT8"
  | `INT16 -> "INT16"
  | `UINT16 -> "UINT16"
  | `INT32 -> "INT32"
  | `FLOAT32 -> "FLOAT32"
  | `FLOAT64 -> "FLOAT64"

(** Get a string representation for a given data type. *)
let ( _type_size_in_bytes : data_t -> int ) = function
    `INT8
  | `UINT8 -> 1
  | `INT16
  | `UINT16 -> 2
  | `INT32 -> 4
  | `FLOAT32 -> 4
  | `FLOAT64 -> 8

(** String to variant type tage. *)
let ( _data_type_from_string : string -> data_t ) = function
    "INT8" -> `INT8
  | "UINT8" -> `UINT8
  | "INT16" -> `INT16
  | "UINT16" -> `UINT16
  | "INT32" -> `INT32
  | "FLOAT32" -> `FLOAT32
  | "FLOAT64" -> `FLOAT64
  | x -> raise (BadDataType ("Invalid or unhandled data type ", x))

(** Get a variant type tag from a Hdf.t *)
let ( _data_type_from_t : t -> data_t ) = function
    Int8 _ -> `INT8
  | UInt8 _ -> `UINT8
  | Int16 _ -> `INT16
  | UInt16 _ -> `UINT16
  | Int32 _ -> `INT32
  | Float32 _ -> `FLOAT32
  | Float64 _ -> `FLOAT64

(** [dims data] returns an array holding the dimensions of [data]. *)
let dims data =
  let f x = Bigarray.Genarray.dims x in
  match data with
      Int8 x -> f x
    | UInt8 x -> f x
    | Int16 x -> f x
    | UInt16 x -> f x
    | Int32 x -> f x
    | Float32 x -> f x
    | Float64 x -> f x

(** [sub data initial_index length] returns a subsection of [data] of length
    [length] starting from [initial_index].
*)
let sub data initial_index length =
  let f x = Bigarray.Genarray.sub_left x initial_index length in
  match data with
      Int8 x -> Int8 (f x)
    | UInt8 x -> UInt8 (f x)
    | Int16 x -> Int16 (f x)
    | UInt16 x -> UInt16 (f x)
    | Int32 x -> Int32 (f x)
    | Float32 x -> Float32 (f x)
    | Float64 x -> Float64 (f x)

(** [reshape data dims] works in exactly the same way as Bigarray.reshape *)
let reshape data dims =
  let f x = Bigarray.reshape x dims in
  match data with
      Int8 x -> Int8 (f x)
    | UInt8 x -> UInt8 (f x)
    | Int16 x -> Int16 (f x)
    | UInt16 x -> UInt16 (f x)
    | Int32 x -> Int32 (f x)
    | Float32 x -> Float32 (f x)
    | Float64 x -> Float64 (f x)

(** [slice data new_dimensions] returns a subarray of [data] with dimensions
    [new_dimensions].
*)
let slice data new_dimensions =
  let f x = Bigarray.Genarray.slice_left x new_dimensions in
  match data with
      Int8 x -> Int8 (f x)
    | UInt8 x -> UInt8 (f x)
    | Int16 x -> Int16 (f x)
    | UInt16 x -> UInt16 (f x)
    | Int32 x -> Int32 (f x)
    | Float32 x -> Float32 (f x)
    | Float64 x -> Float64 (f x)

(** [blit source dest] copies the data from [source] to [dest].
*)
let blit source dest =
  let f x y = Bigarray.Genarray.blit x y in
  match (source, dest) with
      (Int8 x, Int8 y) -> f x y
    | (UInt8 x, UInt8 y) -> f x y
    | (Int16 x, Int16 y) -> f x y
    | (UInt16 x, UInt16 y) -> f x y
    | (Int32 x, Int32 y) -> f x y
    | (Float32 x, Float32 y) -> f x y
    | (Float64 x, Float64 y) -> f x y
    | _ -> raise (BadDataType ("blit", ""))

(** [get_* data which_datum] returns a single element from [data] at
    the location specified by the array [which_datum].
*)
let get_int data which_datum =
  let f x = Bigarray.Genarray.get x which_datum in
  match data with
    | Int8 x -> f x
    | UInt8 x -> f x
    | Int16 x -> f x
    | UInt16 x -> f x
    | _ -> raise (BadDataType ("get_int", ""))

let get_int32 data which_datum =
  let f x = Bigarray.Genarray.get x which_datum in
  match data with
    | Int32 x -> f x
    | _ -> raise (BadDataType ("get_int32", ""))

let get_float data which_datum =
  let f x = Bigarray.Genarray.get x which_datum in
  match data with
    | Float32 x -> f x
    | Float64 x -> f x
    | _ -> raise (BadDataType ("get_float", ""))

(** [set_* data which_datum value] sets a single element of [data] at
    the location specified by the array [which_datum] to [value].
*)
let set_int data which_datum value =
  let f x = Bigarray.Genarray.set x which_datum value in
  match data with
    | Int8 x -> f x
    | UInt8 x -> f x
    | Int16 x -> f x
    | UInt16 x -> f x
    | _ -> raise (BadDataType ("set_int", ""))

let set_int32 data which_datum value =
  let f x = Bigarray.Genarray.set x which_datum value in
  match data with
    | Int32 x -> f x
    | _ -> raise (BadDataType ("set_int32", ""))

let set_float data which_datum value =
  let f x = Bigarray.Genarray.set x which_datum value in
  match data with
    | Float32 x -> f x
    | Float64 x -> f x
    | _ -> raise (BadDataType ("set_float", ""))

(** {6 Convenience, Array-module-like functions} *)

(** Number of elements in the whole data set. *)
let elems b =
  let dims = dims b in
  Array.fold_left ( * ) 1 dims

(** [apply_* f b] applies [f] to every element of [b], modifying [b] in place. *)
let apply_int f data =
  match data with
    | Int8 x -> G.apply f x
    | UInt8 x -> G.apply f x
    | Int16 x -> G.apply f x
    | UInt16 x -> G.apply f x
    | _ -> raise (BadDataType ("apply_int", ""))

let apply_int32 f data =
  match data with
    | Int32 x -> G.apply f x
    | _ -> raise (BadDataType ("apply_int32", ""))

let apply_float f data =
  match data with
    | Float32 x -> G.apply f x
    | Float64 x -> G.apply f x
    | _ -> raise (BadDataType ("apply_float", ""))

(** [map_* f kind b] applies [f] to every element of [b], returning the resulting values.
    [b] is not changed.
*)
let map_int f kind data =
  match data with
    | Int8 x -> G.map f kind x
    | UInt8 x -> G.map f kind x
    | Int16 x -> G.map f kind x
    | UInt16 x -> G.map f kind x
    | _ -> raise (BadDataType ("map_int", ""))

let map_int32 f kind data =
  match data with
    | Int32 x -> G.map f kind x
    | _ -> raise (BadDataType ("map_int32", ""))

let map_float f kind data =
  match data with
    | Float32 x -> G.map f kind x
    | Float64 x -> G.map f kind x
    | _ -> raise (BadDataType ("map_float", ""))

(** [fold_* f initial b]
    Like Array.fold_left and List.fold_left, but over an entire data set.
    Note: The Bigarray is flattened, so this function works over all of
    the elements, regardless of the Bigarray dimensions.
*)
let fold_int f initial data =
  match data with
    | Int8 x -> G.fold f initial x
    | UInt8 x -> G.fold f initial x
    | Int16 x -> G.fold f initial x
    | UInt16 x -> G.fold f initial x
    | _ -> raise (BadDataType ("fold_int", ""))

let fold_int32 f initial data =
  match data with
    | Int8 x -> G.fold f initial x
    | _ -> raise (BadDataType ("fold_int32", ""))

let fold_float f initial data =
  match data with
    | Float32 x -> G.fold f initial x
    | Float64 x -> G.fold f initial x
    | _ -> raise (BadDataType ("fold_float", ""))

module SD =
struct
  type interface_id = InterfaceID of int32
  type data_id = DataID of int32
  (** This type holds the basic specs for SDS entries in a HDF4 file. *)
  type data_spec = {
    index : data_id;
    name : string;
    data_type : data_t;
    dimensions : int array;
  }

  type sds_t = {
    sds_name : string;
    sds_type : data_t;
    sds_dimensions : int array;
    data : t;
  }

  external c_open_file: string -> int32 = "ml_SDstart"

  (** Open the Scientific Data Set (SDS) data interface for a HDF4 file.
      [open_file filename] returns the sd_id for [filenames]'s interface.
      This function opens the SDS interface in read-only mode.
  *)
  let open_file filename = InterfaceID (c_open_file filename)

  external c_get_info: int32 -> int32 -> (int32 * string * string * int array * int) = "ml_SDgetinfo"
  (** Get specs for an individual SDS entry.
      [get_sd_info sd_id sd_index] gives [(sd_index, SDS name, data type string,
      array of dimensions, num_attrs)]
  *)

  (** [get_info interface_id data_id] - Get specs for the given SDS entry. *)
  let get_info (InterfaceID sd_id) (DataID i) =
    let (sd_index, name, data_type_string, dimension_array, num_attrs) =
      c_get_info sd_id i
    in
    {
      index = DataID sd_index;
      name = name;
      data_type = _data_type_from_string data_type_string;
      dimensions = dimension_array
    }

  (** [c_get_data sd_id sd_index genarray] returns a string with the data type.
      The actual data is stored in [genarray].
  *)
  external c_get_data: int32 -> int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
    string = "ml_SDreaddata"

  let get_data (InterfaceID sd_id) (DataID sd_index) data =
    c_get_data sd_id sd_index data
  (** [get_data sd_id sd_index genarray] returns a string with the data type.
      The actual data is stored in [genarray].
  *)

  external c_close_file: int32 -> int = "ml_SDend"

  (** [close_file sd_id] closes the given SDS interface.
      The return value indicates success or failure.  But this will change to
      a return value of unit and the function will then throw an exception when
      there is an error.
  *)
  let close_file (InterfaceID cs_id) = c_close_file cs_id

  (** {6 Pure OCaml functions and wrappers}
      These functions make a prettier, and hopefully more functional, interface to
      underlying C library calls.
  *)

  (** [get_spec_list sd_id] returns a list of the information given by {!Hdf.get_sd_info}
      for each SDS available through the given [sd_id] interface.
  *)
  let get_spec_list sd_id =
    let rec f i =
      try
        let info = get_info sd_id (DataID i) in
        info :: f (Int32.add i 1l)
      with
          (* TODO - This isn't a very good error checking method. *)
        Failure x ->
          if i > 0l then
            []
          else
            failwith x
    in
    f 0l

  (** [get_index_from_name sd_id name] will return the index of the SDS named [name]. *)
  let get_index_from_name sd_id name =
    (List.find (fun x -> x.name = name) (get_spec_list sd_id)).index

  (** Actually GET the data in to the bigarray. *)
  let _get_data sd_id sd_index data =
    let f x = get_data sd_id sd_index x in
    match data with
        Int8 x -> f x
      | UInt8 x -> f x
      | Int16 x -> f x
      | UInt16 x -> f x
      | Int32 x -> f x
      | Float32 x -> f x
      | Float64 x -> f x

  (** [get_by_name interface_id data_name] returns the data called [data_name]. *)
  let get_data_by_name sd_id name =
    let sd_index = get_index_from_name sd_id name in
    let specs = get_info sd_id sd_index in
    let data = create specs.data_type specs.dimensions in
    let _ = _get_data sd_id sd_index data in
    data

  (** [get_by_name interface_id data_name] returns the data called [data_name]. *)
  let get_data_by_index sd_id sds_index =
    let specs = get_info sd_id sds_index in
    let data = create specs.data_type specs.dimensions in
    let _ = _get_data sd_id sds_index data in
    data

  (** [read_sds_t_list sd_id] returns a list of the SDS contents of [sd_id] *)
  let read_sds_t_list (InterfaceID sd_id) =
    let rec f i =
      try
        let info = get_info (InterfaceID sd_id) (DataID i) in
        let this_sds = { sds_name = info.name; sds_type = info.data_type;
                         sds_dimensions = info.dimensions;
                         data = get_data_by_index (InterfaceID sd_id) info.index;
                       }
        in
        this_sds :: f (Int32.add i 1l)
      with
          (* TODO - This isn't a very good error checking method. *)
        Failure x ->
          if i > 0l then
            []
          else
            failwith x
    in
    f 0l

  external c_open_sd_create: string -> int32 = "ml_SDstart_create"

  (** [open_sd_create filename] creates a new HDF4 file [filename] with an SDS interface.
  *)
  let open_sd_create filename = InterfaceID (c_open_sd_create filename)

  (** [c_create sd_id name datatype_string number_of_dimensions genarray_of_dimensions] *)
  external c_create: int32 -> string -> string -> int32 ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> int32 = "ml_SDcreate"

  (** [create sd_id name data_type dimensions] creates an entry in [sd_id] named
      [name] of type [data_type] with the given [dimensions].
  *)
  let create (InterfaceID sd_id) name data_type dimensions =
    let dims =
      Bigarray.genarray_of_array1
      (Bigarray.Array1.of_array Bigarray.int Bigarray.c_layout dimensions)
    in
    let data_type_string = _string_from_data_type data_type in
    DataID (c_create sd_id name data_type_string (Int32.of_int (Array.length dimensions)) dims)

  external c_write_data: int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit = "ml_SDwritedata"

  (** [write_data sds_id data] creates an entry and writes [data] to the file
      referenced by [sd_id].
  *)
  let write_data (DataID sds_id) data =
    let f x = c_write_data sds_id x in
    match data with
        Int8 x -> f x
      | UInt8 x -> f x
      | Int16 x -> f x
      | UInt16 x -> f x
      | Int32 x -> f x
      | Float32 x -> f x
      | Float64 x -> f x

  (** [create_data sd_id name data] - create and write an SDS named [name] *)
  let create_data sd_id name data =
    let sds_id = create sd_id name (_data_type_from_t data) (dims data) in
    write_data sds_id data

  external c_end_access: int32 -> int = "ml_SDendaccess"

  (** [end_access sds_id]
      See above comments about return values...
  *)
  let end_access (DataID sds_id) = c_end_access sds_id
end

module Vdata =
struct
  type interface_id = InterfaceID of int32
  type data_id = DataID of int32

  (** This data type holds everything about a given Vdata, including the
      contents as raw bytes *)
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

  (** Allocate space to hold Vdata *)
  let allocate_vdata_bigarray bytes =
    Genarray.create int8_unsigned c_layout [|bytes|]

  (** [read_vdata_t vdata_id] returns the given Vdata, along with specs. *)
  let read_vdata_t vdata_id =
    let (istat, n_records, interlace, fields, vdata_size, vdata_name) =
      vs_inquire vdata_id
    in
    let vdata_class = vs_getclass vdata_id in
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
      allocate_vdata_bigarray (Int32.to_int vdata_size * Int32.to_int n_records)
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

  (** [vdata_read_map f file_id] will go through each Vdata in [file_id],
      applying [f] and returning a list of the results.  This is mainly meant
      to be used in the [read_vdata_t_list] function below but may have other
      uses.
  *)
  let vdata_read_map f file_id =
    let vdata_ref_0 = vs_getid file_id (-1l) in
    let rec loop l vdata_ref =
      let vdata_id = vs_attach file_id vdata_ref "r" in
      if vdata_id = -1l then
        List.rev l
      else
        let result = f vdata_id in
        let () = vs_detach vdata_id in
        loop (result :: l) (vs_getid file_id vdata_ref)
    in
    loop [] vdata_ref_0

  (** [read_vdata_t_list filename] will return a list of [vdata_t], one for each
      Vdata in [filename].  The data in the returned list are in the same order
      as those in [filename].

  *)
  let read_vdata_t_list filename =
    let file_id = h_open filename DFACC_READ 0 in
    v_start file_id;

    let vdata_list = vdata_read_map read_vdata_t file_id in

    v_end file_id;
    h_close file_id;
    vdata_list

  (** [write_vdata_t file_id data] will write out the Vdata+specs given in [data].
      The combination of [read_vdata_t] and [write_vdata_t] should preserve the
      original Vdata structure in the new file.
  *)
  let write_vdata_t file_id data =
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
    ()

  (** [write_vdata_t_list file_id vdata_list] will write out each element of
      [vdata_list] to [file_id] in order.
  *)
  let rec write_vdata_t_list file_id vdata_list =
    match vdata_list with
        hd :: tl ->
          write_vdata_t file_id hd;
          write_vdata_t_list file_id tl
      | [] ->
          ()

  (** This type holds the basic specs for SDS entries in a HDF4 file. *)
  type data_spec = {
    index : data_id;
    name : string;
    field_name_list : string;
    data_size : int; (* Size (in bytes) of each record. *)
    records : int;   (* Number of records. *)
  }

  (** {6 Native C interface}
      The following are the raw calls to the C functions.  Most, if not all, should
      be wrapped in appropriate OCaml goodness. *)

  external c_open_hdf: string -> int32 = "ml_Hopen"

  (** [open_hdf filename] returns the file_id for the Vdata interface of the
      HDF4 file [filename].
      This opens the file in read-only mode.
  *)
  let open_hdf filename = InterfaceID (c_open_hdf filename)

  external c_close_hdf: int32 -> int = "ml_Hclose"

  (** [close_hdf file_id] closes the given HDF4 interface referenced by file_id.
      See {!Hdf.close_sd} for a discussion on the return value.
  *)
  let close_hdf (InterfaceID file_id) = c_close_hdf file_id

  external c_start: int32 -> int32 = "ml_Vstart"

  (** [start file_id] gives the Vdata index number for the given HDF interface.
  *)
  let start (InterfaceID file_id) = DataID (c_start file_id)

  (** [c_get_info file_id vd_index] retrieves information on the data referenced
      by vd_index.
      The return value is [(vd_index, name, field name list, data size, number of records)]
  *)
  external c_get_info: int32 -> int32 -> (int32 * string * string * int * int) = "ml_VSinquire"

  (** [get_info file_id vd_index] *)
  let get_info (InterfaceID file_id) (DataID vd_index) =
    let (index, name, field_name_list, data_size, records) =
      c_get_info file_id vd_index
    in
    {
      index = DataID index;
      name = name;
      field_name_list = field_name_list;
      data_size = data_size;
      records = records;
    }

  (** [get_spec_list file_id] returns a list of the information given by {!Hdf.get_vs_info}
      for each Vdata available through the given [file_id] interface.
  *)
  let get_spec_list file_id =
    let rec f i =
      let x = Int32.of_int i in
      try
        let info = get_info file_id (DataID x) in
        info :: f (i + 1)
      with
          Failure x ->
            if i > 0 then
              []
            else
              failwith x
    in
    f 0

  (** [get_VDdata file_id vd_index genarray] returns the data type string for [vd_index].
      The data referenced by [vd_index] is stored in [genarray].
      XXX : This is an older, more complex version.  The "raw" VSread function is wrapped
      as [Hdf.vs_read].
  *)
  external c_get_data: int32 -> int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
    string = "ml_VSread_complicated"

  (** Actually GET the data in to the bigarray. *)        
  let _get_data (InterfaceID file_id) (DataID vd_index) data =
    let f x = c_get_data file_id vd_index x in
    match data with
        Int8 x -> f x
      | UInt8 x -> f x
      | Int16 x -> f x
      | UInt16 x -> f x
      | Int32 x -> f x
      | Float32 x -> f x
      | Float64 x -> f x

  external c_vd_index_from_vdata_ref: int32 -> int32 -> int32 = "ml_vd_index_from_vdata_ref"

  (** Get the index from a vdata_ref value. *)
  let vd_index_from_vdata_ref (InterfaceID file_id) (DataID vdata_ref) =
    DataID (c_vd_index_from_vdata_ref file_id vdata_ref)

  external c_VSfind: int32 -> string -> int32 = "ml_VSfind"

  (** [get_sd_index_from_name sd_id name] will return the index of the SDS named [name]. *)
  let get_index_from_name (InterfaceID file_id) name =
    (* (List.find (fun x -> x.name = name) (get_spec_list file_id)).index *)
    (* DataID (c_get_index_from_name file_id name) *)
    vd_index_from_vdata_ref (InterfaceID file_id) (DataID (c_VSfind file_id name))

  (** [get_by_name interface_id data_name] returns the data called [data_name]. *)
  let get_data_by_name file_id name data_type =
    let vd_index = get_index_from_name file_id name in
    let specs = get_info file_id vd_index in
    let size_multiplier = specs.data_size / (_type_size_in_bytes data_type) in
    (* TODO FIXME XXX - Change this to throw an error if
       specs.records * specs.data_size ne specs.records * size_multiplier
       AND
       if size_multiplier = 0, or other possible bad outcomes.
    *)
    let data =
      create data_type [| specs.records * size_multiplier |]
    in
    let _ = _get_data file_id vd_index data in
    data

  (** [get_by_name interface_id data_name] returns the data called [data_name]. *)
  let get_data_by_index file_id vd_index data_type =
    let specs = get_info file_id vd_index in
    let size_multiplier = specs.data_size / (_type_size_in_bytes data_type) in
    (* TODO FIXME XXX - Change this to throw an error if
       specs.records * specs.data_size ne specs.records * size_multiplier
       AND
       if size_multiplier = 0, or other possible bad outcomes.
    *)
    let data =
      create data_type [| specs.records * size_multiplier |]
    in
    let _ = _get_data file_id vd_index data in
    data

  external c_v_end: int32 -> int = "ml_Vend"

  (** [v_end file_id] closes the Vdata interface to [file_id].
      See {!Hdf.close_sd} for a discussion on the return value.
  *)
  let v_end (InterfaceID file_id) = c_v_end file_id

  (** {6 File and data set creation functions} *)

  (** [open_hdf_create filename] creates the HDF4 file [filename].
      [filename] must exist.
      File is opened read-only.
      See above comments about return values...
  *)
  external open_hdf_create: string -> int = "ml_Hopen_create"

  external c_open_hdf_rw: string -> int32 = "ml_Hopen_rw"

  (** Like {!Hdf.open_hdf_create} but file is opened read-write. *)
  let open_hdf_rw filename = InterfaceID (c_open_hdf_rw filename)

  (** NOTE: [vdata_id] values must be [Int32] values, as the HDF library uses the full 32 range. *)

  external c_create: int32 -> int32 = "ml_VSattach_create"

  (** [create file_id] returns a vdata_id for a new Vdata entry in the HDF4 file
      referenced by [file_id].
  *)
  let create (InterfaceID file_id) = DataID (c_create file_id)

  external c_define_fields: int32 -> string -> string -> int -> unit = "ml_VSfdefine"

  (** [define_fields vdata_id fieldname datatype_string order] *)
  let define_fields (DataID vdata_id) fieldname data_type order =
    c_define_fields vdata_id fieldname (_string_from_data_type data_type) order

  external c_set_fields: int32 -> string -> unit = "ml_VSsetfields"

  (** [set_fields vdata_id field_list_string] *)
  let set_fields (DataID vdata_id) field_list_string = c_set_fields vdata_id field_list_string

  (** [write vdata_id data number_of_records] *)
  let write (DataID vdata_id) data number_of_records =
    let f x = vs_write vdata_id x number_of_records in
    match data with
        Int8 x -> f x
      | UInt8 x -> f x
      | Int16 x -> f x
      | UInt16 x -> f x
      | Int32 x -> f x
      | Float32 x -> f x
      | Float64 x -> f x

  external c_set_name: int32 -> string -> unit = "ml_VSsetname"

  (** [set_name vdata_id name] *)
  let set_name (DataID vdata_id) name = c_set_name vdata_id name

  external c_set_class: int32 -> string -> unit = "ml_VSsetclass"

  (** [set_class vdata_id class] *)
  let set_class (DataID vdata_id) name = c_set_class vdata_id name

  external c_detach: int32 -> unit = "ml_VSdetach"

  (** [vs_detach vdata_id]
      Detach the Vdata interface.
  *)
  let detach (DataID vdata_id) = c_detach vdata_id
end

