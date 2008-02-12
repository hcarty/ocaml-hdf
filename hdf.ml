(** {5 HDF4} *)

open Bigarray
module G = ExtBigarray

(** {6 Low Level Functions} *)

include Hdf_wrapper

(** Carry over some naming conventions from hdf.h *)
let hdf_open = h_open
let hdf_close = h_close
let v_start = v_initialize
let v_end = v_finish
let he_clear = hep_clear

(** A somewhat redundant type, used for communication with the C library *)
type data_t = [ `int8 | `uint8 | `int16 | `uint16 | `int32 | `float32 | `float64 ]

(** Convert HDF data type to a data_t *)
external hdf_datatype_to_mlvariant: int32 -> data_t = "hdf_datatype_to_mlvariant"

(** Low-level functions wrapped by hand to read and write data. *)
external vs_write: int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  int32 -> unit = "ml_VSwrite"
external vs_read: int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  int32 -> unit = "ml_VSread"

(** [sd_writedata sdsid data] *)
external sd_writedata: int32 ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit = "ml_SDwritedata"

(** [sd_readdata sdsid start end data] *)
external sd_readdata: int32 -> int32 array -> int32 array ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit = "ml_SDreaddata"

(** SDS and file-wide attribute reading/writing *)
external sd_readattr: int32 -> int32 ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit = "ml_SDreadattr"
external sd_setattr: int32 -> string -> int32 -> int32 ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit = "ml_SDsetattr"

(** SDS fill value reading/writing *)
external sd_getfillvalue_float: int32 -> float = "ml_SDgetfillvalue_float"
external sd_getfillvalue_int: int32 -> int = "ml_SDgetfillvalue_int"
external sd_getfillvalue_int32: int32 -> int32 = "ml_SDgetfillvalue_int32"
external sd_setfillvalue_float: int32 -> float -> unit = "ml_SDsetfillvalue_float"
external sd_setfillvalue_int: int32 -> int -> unit = "ml_SDsetfillvalue_int"
external sd_setfillvalue_int32: int32 -> int32 -> unit = "ml_SDsetfillvalue_int32"

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

(** Exception raised if we try to get at data which we shouldn't or can't handle. *)
exception BadDataType of string * string

(** Exception raised if reading or writing data fails *)
exception HdfError of string

(** {6 Internal library functions } *)

(** Check to see if a file is a valid HDF4 data file.
    [is_hdf filename] give true if [filename] is a HDF4 file, false if not.
    TODO - Remove this, it's in the lowlevel routines.
*)
let is_hdf filename = match h_ishdf filename with 1 -> true | _ -> false

(** Create a bigarray of the appropriate type, wrapped as an [Hdf.t]. *)
let create (data_type : data_t) dimensions =
  let f x = Bigarray.Genarray.create x Bigarray.c_layout dimensions in
  match data_type with
      `int8 -> Int8 (f Bigarray.int8_signed)
    | `uint8 -> UInt8 (f Bigarray.int8_unsigned)
    | `int16 -> Int16 (f Bigarray.int16_signed)
    | `uint16 -> UInt16 (f Bigarray.int16_unsigned)
    | `int32 -> Int32 (f Bigarray.int32)
    | `float32 -> Float32 (f Bigarray.float32)
    | `float64 -> Float64 (f Bigarray.float64)

(** Get a string representation for a given data type. *)
let ( _type_size_in_bytes : data_t -> int ) = function
    `int8
  | `uint8 -> 1
  | `int16
  | `uint16 -> 2
  | `int32 -> 4
  | `float32 -> 4
  | `float64 -> 8

(** Get a variant type tag from a Hdf.t *)
let ( _data_type_from_t : t -> data_t ) = function
    Int8 _ -> `int8
  | UInt8 _ -> `uint8
  | Int16 _ -> `int16
  | UInt16 _ -> `uint16
  | Int32 _ -> `int32
  | Float32 _ -> `float32
  | Float64 _ -> `float64

(** Get a HDF type from a Hdf.t *)
let _hdf_type_from_t = function
    Int8 _ -> DFNT_INT8
  | UInt8 _ -> DFNT_UINT8
  | Int16 _ -> DFNT_INT16
  | UInt16 _ -> DFNT_UINT16
  | Int32 _ -> DFNT_INT32
  | Float32 _ -> DFNT_FLOAT32
  | Float64 _ -> DFNT_FLOAT64

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

  (** [read_data ?name ?index interface] -
      Must provide ONE of [name] OR [index].  It return an object containing
      the SDS contents and related metadata. *)
  let read_data ?name ?index interface =
    let sds_id =
      match name, index with
          None, None
        | Some _, Some _ -> raise (Invalid_argument "Hdf.SD.read_data")
        | Some n, None -> sd_select interface (sd_nametoindex interface n)
        | None, Some i -> sd_select interface i
    in

    (* Remember only the first [rank] elements of [dimsizes] actually mean
       anything for the SDS being read. *)
    let (sds_name, rank, dimsizes, data_type, num_attrs) =
      sd_getinfo sds_id
    in

    (* Create the Hdf.t which will hold the SDS contents *)
    let data =
      create
        (hdf_datatype_to_mlvariant data_type)
        (Array.init (Int32.to_int rank) (fun i -> Int32.to_int dimsizes.(i)))
    in

    (* Always read the entire data set.  The "stride" option is set to NULL. *)
    let start = Array.make (Int32.to_int rank) 0l in
    let edges = Array.init (Int32.to_int rank) (fun i -> dimsizes.(i)) in

    let f x = sd_readdata sds_id start edges x in
    let () =
      match data with
          Int8 x -> f x
        | UInt8 x -> f x
        | Int16 x -> f x
        | UInt16 x -> f x
        | Int32 x -> f x
        | Float32 x -> f x
        | Float64 x -> f x
    in

    sd_endaccess sds_id;
    (
      object
        method data = data
        method name = sds_name
        method dimsizes = Array.sub dimsizes 0 (Int32.to_int rank)
        method data_type = data_type
        method num_attrs = num_attrs
      end
    )

  (** [get_spec_list sd_id] returns a list of the information given by {!sd_getinfo}
      for each SDS available through the given [sd_id] interface.
  *)
  let get_spec_list interface =
    let rec f i =
      try
        let sds_id = sd_select interface i in
        let info = sd_getinfo sds_id in
        sd_endaccess sds_id;
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

  (** [read_all sdid] returns a list of the SDS contents of [sdid] *)
  let read_all interface =
    let rec f index l =
      try
        f (Int32.add index 1l) (read_data ~index interface :: l)
      with
          (* TODO - This isn't a very good error checking method. *)
        Failure x ->
          let error_id = he_value 0l in
          if index > 0l && error_id = DFE_NONE then
            List.rev l
          else
            raise (HdfError ("Hdf.SD.read_all: " ^ (he_string error_id)))
    in
    f 0l []

  (** [write_data sds_id data] creates an entry and writes [data] to the file
      referenced by [sds_id].
  *)
  let write_data sds_id data =
    let f x = sd_writedata sds_id x in
    match data with
        Int8 x -> f x
      | UInt8 x -> f x
      | Int16 x -> f x
      | UInt16 x -> f x
      | Int32 x -> f x
      | Float32 x -> f x
      | Float64 x -> f x

  (** [create_data sd_id name data] - create and write an SDS named [name] *)
  let create_data interface name sds =
    match 
      sd_create interface name (_hdf_type_from_t sds)
        (Array.map Int32.of_int (dims sds))
    with
        (-1l) ->
          let error_str = he_string (he_value 0l) in
          raise (
            HdfError
              ("Hdf.SD.create_data: unable to create SDS entry - " ^ error_str)
          )
      | sds_id ->
          write_data sds_id sds;
          sd_endaccess sds_id
end

module Vdata =
struct
  type interface_id = InterfaceID of int32
  type data_id = DataID of int32

  (** Allocate space to hold Vdata *)
  let allocate_vdata_bigarray bytes =
    Genarray.create int8_unsigned c_layout [|bytes|]

  let ref_from_index file_id index =
    let rec f i vdata_ref =
      let new_ref = vs_getid file_id vdata_ref in
      if i = index then
        new_ref
      else
        f (i + 1) new_ref
    in
    let vdata_ref = f 0 (-1l) in
    if vdata_ref = -1l then
      raise (Invalid_argument "Hdf.Vdata.ref_from_index")
    else
      vdata_ref

  let index_from_ref file_id target_ref =
    let rec f i vdata_ref =
      let new_ref = vs_getid file_id vdata_ref in
      let () =
        if vdata_ref = -1l then
          raise (Invalid_argument "Hdf.Vdata.ref_from_index")
        else
          ()
      in
      if new_ref = target_ref then
        i
      else
        f (i + 1) new_ref
    in
    f 0 (-1l)

  (** [read_vdata_by_id vdata_id] returns the given Vdata, along with specs. *)
  let read_data_by_id vdata_id =
    let (n_records, interlace, fields, vdata_size, vdata_name) =
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
    
    (
      object
        method n_records = n_records
        method interlace = interlace
        method fields = fields
        method size = vdata_size
        method name = vdata_name
        method vdata_class = vdata_class
        method num_attrs = num_attrs
        method num_fields = num_fields
        method field_orders = field_orders
        method field_types = field_types
        method field_sizes = field_sizes
        method field_num_attrs = field_num_attrs
        method data = data
      end
    )

  (** [read_data ~name interface] *)
  let read_data ~name interface =
    let vdata_id =
      let vdata_ref = vs_find interface name in
      vs_attach interface vdata_ref "r"
    in
    let vdata = read_data_by_id vdata_id in
    vs_detach vdata_id;
    vdata

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

  (** [read_all file_id] will return a list of Vdata objects, one for each
      Vdata in [file_id].  The data in the returned list are in the same order
      as those in [file_id].

  *)
  let read_all interface =
    vdata_read_map read_data_by_id interface

  (** [write_vdata file_id data] will write out the Vdata+specs given in [data].
      The combination of [read_vdata_t] and [write_vdata_t] should preserve the
      original Vdata structure in the new file.
  *)
  let write_data file_id data =
    let vdata_id = vs_attach file_id (-1l) "w" in
    let field_name_array = Array.of_list (Pcre.split ~pat:"," data#fields) in
    let field_defs =
      Array.init (Array.length data#field_types)
        (fun i -> ( data#field_types.(i), field_name_array.(i), data#field_orders.(i) ))
    in
    Array.iter
      (fun (ftype, fname, forder) -> vs_fdefine vdata_id fname ftype forder)
      field_defs;

    vs_setname vdata_id data#name;
    vs_setclass vdata_id data#vdata_class;
    vs_setfields vdata_id data#fields;

    vs_write vdata_id data#data data#n_records;

    vs_detach vdata_id;
    ()

  (** [write_vdata_list file_id vdata_list] will write out each element of
      [vdata_list] to [file_id] in order.
  *)
  let rec write_data_list file_id vdata_list =
    match vdata_list with
        hd :: tl ->
          write_data file_id hd;
          write_data_list file_id tl
      | [] ->
          ()

  (*
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
  *)
end

