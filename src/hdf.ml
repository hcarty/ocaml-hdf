(** {5 HDF4} *)

open Batteries
open Bigarray

module Genarray = struct
  include Genarray
  include ExtGenarray
end

(** Local support functions *)

type 'a try_t =
  | Result of 'a
  | Error of exn

(** [try_finally it cleanup f] applies [f it] and calls [cleanup it], even if
    [f it] throws an exception.  Any exception thrown by [f it] will be
    propagated along after the call to [cleanup it]. *)
let try_finally it cleanup f =
  let result =
    try
      Result (f it)
    with
    | except -> Error except
  in
  cleanup it;
  match result with
  | Result v -> v
  | Error e -> raise e

(** [unless except f x] returns [Some (f x)] unless [f x] raises the exception
    [except], in which case it returns [None]. *)
let unless except f x =
  try
    Some (f x)
  with
  | e when e = except -> None

(** [a |? b] returns [b] if [a = None], otherwise it returns the contents of
    [a]. *)
let ( |? ) a b = Option.default b a

(** This allows the {!Hdf4} module to use either Fortran-style or C-style
    Bigarrays for data storage. *)
module type HDF4_LAYOUT_TYPE = sig
  type t

  val layout : t Bigarray.layout

  val sub : ('a, 'b, t) Genarray.t -> int -> int -> ('a, 'b, t) Genarray.t
  val slice : ('a, 'b, t) Genarray.t -> int array -> ('a, 'b, t) Genarray.t
end

module C_layout = struct
  type t = c_layout
  let layout = c_layout
  let sub = Genarray.sub_left
  let slice = Genarray.slice_left
end

module Fortran_layout = struct
  type t = fortran_layout
  let layout = fortran_layout
  let sub = Genarray.sub_right
  let slice = Genarray.slice_right
end

(** {6 Low Level Functions}
    This module includes relatively "bare metal" functions from the HDF4 API.
    Note that these are largely unsafe!  If they are not used with care they
    can result in segmentation faults or other unexpected outcomes. *)
module Hdf4_low_level = struct
  include Hdf_wrapper

  (** Carry over some naming conventions from hdf.h *)
  let hdf_open = h_open
  let hdf_close = h_close
  let v_start = v_initialize
  let v_end = v_finish
  let he_clear = hep_clear
  type hdf_vdata_interlace_t =
    | HDF_NO_INTERLACE
    | HDF_FULL_INTERLACE

  (** Low-level functions wrapped by hand to read and write data. *)
  external vs_write: int32 -> ('a, 'b, 'c) Bigarray.Genarray.t ->
    int32 -> hdf_vdata_interlace_t -> unit = "ml_VSwrite"
  external vs_read: int32 -> ('a, 'b, 'c) Bigarray.Genarray.t ->
    int32 -> hdf_vdata_interlace_t -> unit = "ml_VSread"

  (** [sd_writedata sdsid data] *)
  external sd_writedata: int32 ->
    ('a, 'b, 'c) Bigarray.Genarray.t -> unit = "ml_SDwritedata"

  (** [sd_readdata sdsid start end data] *)
  external sd_readdata: int32 -> int32 array -> int32 array ->
    ('a, 'b, 'c) Bigarray.Genarray.t -> unit = "ml_SDreaddata"

  (** SDS and file-wide attribute reading/writing *)
  external sd_readattr: int32 -> int32 ->
    ('a, 'b, 'c) Bigarray.Genarray.t -> unit = "ml_SDreadattr"
  external sd_setattr: int32 -> string -> int32 -> int32 ->
    ('a, 'b, 'c) Bigarray.Genarray.t -> unit = "ml_SDsetattr"

  (** Vdata attribute reading/writing *)
  external vs_getattr: int32 -> int -> int32 ->
    ('a, 'b, 'c) Bigarray.Genarray.t ->
    unit = "ml_VSgetattr"
  external vs_setattr: int32 -> int32 -> string -> int32 -> int32 ->
    ('a, 'b, 'c) Bigarray.Genarray.t ->
    unit = "ml_VSsetattr_bytecode" "ml_VSsetattr"

  (** SDS fill value reading/writing *)
  external sd_getfillvalue_float: int32 -> float = "ml_SDgetfillvalue_float"
  external sd_getfillvalue_int: int32 -> int = "ml_SDgetfillvalue_int"
  external sd_getfillvalue_int32: int32 -> int32 = "ml_SDgetfillvalue_int32"
  external sd_setfillvalue_float: int32 -> float -> unit =
    "ml_SDsetfillvalue_float"
  external sd_setfillvalue_int: int32 -> int -> unit = "ml_SDsetfillvalue_int"
  external sd_setfillvalue_int32: int32 -> int32 -> unit =
    "ml_SDsetfillvalue_int32"

  (** [sd_getinfo] output can use a little cleanup in the dimsizes *)
  let sd_getinfo sdsid =
    let (name, rank, dimsizes, data_type, num_attrs) = sd_getinfo sdsid in
    (name, rank, Array.sub dimsizes 0 (Int32.to_int rank), data_type, num_attrs)
end

module type MAPPABLE = sig
  type key
  type 'a t

  val empty : 'a t
  val enum : 'a t -> (key * 'a) Enum.t
  val of_enum : (key * 'a) Enum.t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val keys : 'a t -> key Enum.t
  val values : 'a t -> 'a Enum.t
  val find : key -> 'a t -> 'a option
end

(** A complete set of HDF4 modules, parameterized by the underlying
    Bigarray layout (C or Fortran). *)
module Make = functor (Layout : HDF4_LAYOUT_TYPE) -> functor (Smap : MAPPABLE with type key = string) -> struct
  open Hdf4_low_level

  (** {6 Higher Level Functions} *)

  module Hdf4 = struct

    (** A somewhat redundant type, used for communication with the C library *)
    type data_t =
      [ `int8 | `uint8 | `int16 | `uint16 | `int32 | `float32 | `float64 ]

    (** Exception raised if reading or writing data fails *)
    exception HdfError of string

    (** Enclose the interface creation function(s) in a module to ensure
        that an interface can not be manually created. *)
    module Private :
    sig
      (* TODO: Try making the interface ids [int32 option] types,
         rather than indicating a missing interface with [-1l]. *)
      type interface = private { sdid : int32 option; fid : int32 option }
      val open_file : ?access:access_type -> string -> interface
      (* val from_int32s : int32 -> int32 -> interface *)
    end =
    struct
      (** HDF interfaces type *)
      type interface = { sdid : int32 option; fid : int32 option }

      (** [open_file ?access filename] will open the HDF file [filename] with
          [access] permissions (default is [DFACC_READ].  This opens and starts
          both the SDS and Vdata interfaces.

          @raise Invalid_argument if [filename] fails to open. *)
      let open_file ?(access = DFACC_READ) filename =
        let sd_access =
          match access with
          | DFACC_CREATE -> DFACC_WRITE
          | x -> x
        in
        let fid =
          match h_open filename access 0 with
          | -1l -> None
          | i ->
              v_start i;
              Some i
        in
        let sdid =
          match sd_start filename sd_access with
          | -1l -> None
          | i -> Some i
        in
        (* At least one of the interfaces must be available in order to
           provide a useful result. *)
        match fid, sdid with
        | None, None -> invalid_arg filename
        | _ -> { sdid = sdid; fid = fid }

      (** This is a work around - it is not currently used, and probably
          should not be used at all. *)
      (* let from_int32s sdid fid = { sdid = sdid; fid = fid } *)
    end

    include Private

    (** [close_file interface] closes an HDF file opened with [open_file] *)
    let close_file interface =
      (* Only close the interface IF it is actually open *)
      Option.may sd_end interface.sdid;
      Option.may (
        fun i ->
          v_end i;
          h_close i;
      ) interface.fid;
      ()

    (** [open_file_in ?access f file] opens the HDF4 file [file] and passes the
        HDF interface to [f].  After [f] completes or raises an exception,
        [file] will be closed. *)
    let open_file_in ?access f file =
      try_finally
        (open_file ?access file)
        close_file
        (fun hdf_interface -> f hdf_interface)

    (** The basic type which encapsulates the HDF data types we can
        support.  These are encapsulated in a variant type to avoid having
        to write explicit cases for every data type within a given HDF4
        file. *)
    type t =
      | Int8 of (int, int8_signed_elt, Layout.t) Genarray.t
      | UInt8 of (int, int8_unsigned_elt, Layout.t) Genarray.t
      | Int16 of (int, int16_signed_elt, Layout.t) Genarray.t
      | UInt16 of (int, int16_unsigned_elt, Layout.t) Genarray.t
      | Int32 of (int32, int32_elt, Layout.t) Genarray.t
      | Float32 of (float, float32_elt, Layout.t) Genarray.t
      | Float64 of (float, float64_elt, Layout.t) Genarray.t

    (** Create a bigarray of the appropriate type, wrapped as an [Hdf.t]. *)
    let create (data_type : data_t) dimensions =
      let f x = Genarray.create x Layout.layout dimensions in
      match data_type with
      | `int8 -> Int8 (f int8_signed)
      | `uint8 -> UInt8 (f int8_unsigned)
      | `int16 -> Int16 (f int16_signed)
      | `uint16 -> UInt16 (f int16_unsigned)
      | `int32 -> Int32 (f int32)
      | `float32 -> Float32 (f float32)
      | `float64 -> Float64 (f float64)

    (** [to_* data] returns the Genarray.t wrapped in [data]. Raises
        [Invalid_argument] if the wrong data type is provided. *)
    let to_int8 = function
        Int8 x -> x | _ -> invalid_arg "to_int8"
    let to_uint8 = function
        UInt8 x -> x | _ -> invalid_arg "to_uint8"
    let to_int16 = function
        Int16 x -> x | _ -> invalid_arg "to_int16"
    let to_uint16 = function
        UInt16 x -> x | _ -> invalid_arg "to_uint16"
    let to_int32 = function
        Int32 x -> x | _ -> invalid_arg "to_int32"
    let to_float32 = function
        Float32 x -> x | _ -> invalid_arg "to_float32"
    let to_float64 = function
        Float64 x -> x | _ -> invalid_arg "to_float64"

    (** [of_* data] returns a {!t} wrapping [data]. *)
    let of_int8 x = Int8 x
    let of_uint8 x = UInt8 x
    let of_int16 x = Int16 x
    let of_uint16 x = UInt16 x
    let of_int32 x = Int32 x
    let of_float32 x = Float32 x
    let of_float64 x = Float64 x

    (** {6 Internal library functions } *)

    (** Check to see if a file is a valid HDF4 data file.
        [is_hdf filename] give true if [filename] is a HDF4 file, false if not.
        TODO - Remove this, it's in the lowlevel routines. *)
    let is_hdf filename = match h_ishdf filename with 1 -> true | _ -> false

    (** Convert HDF data type to and from a data_t *)
    external hdf_datatype_to_mlvariant: int32 -> data_t =
      "hdf_datatype_to_mlvariant"
    external mlvariant_to_hdf_datatype: data_t -> int32 =
      "mlvariant_to_hdf_datatype"

    (** Get a string representation for a given data type. *)
    let ( _type_size_in_bytes : data_t -> int ) = function
      | `int8
      | `uint8 -> 1
      | `int16
      | `uint16 -> 2
      | `int32 -> 4
      | `float32 -> 4
      | `float64 -> 8

    (** Get a variant type tag from a Hdf.t *)
    let ( _data_type_from_t : t -> data_t ) = function
      | Int8 _ -> `int8
      | UInt8 _ -> `uint8
      | Int16 _ -> `int16
      | UInt16 _ -> `uint16
      | Int32 _ -> `int32
      | Float32 _ -> `float32
      | Float64 _ -> `float64

    (** Get a variant type tag from a HDF type *)
    let ( _data_type_from_hdf_type : hdf_data_type -> data_t ) = function
      | DFNT_INT8 -> `int8
      | DFNT_UINT8 -> `uint8
      | DFNT_INT16 -> `int16
      | DFNT_UINT16 -> `uint16
      | DFNT_INT32 -> `int32
      | DFNT_FLOAT32 -> `float32
      | DFNT_FLOAT64 -> `float64
      | _ -> raise (Invalid_argument "Unhandled HDF data type")

    (** Get a HDF type from a Hdf.t *)
    let _hdf_type_from_t = function
      | Int8 _ -> DFNT_INT8
      | UInt8 _ -> DFNT_UINT8
      | Int16 _ -> DFNT_INT16
      | UInt16 _ -> DFNT_UINT16
      | Int32 _ -> DFNT_INT32
      | Float32 _ -> DFNT_FLOAT32
      | Float64 _ -> DFNT_FLOAT64

    type 'u apply_f_t = {
      f : 'a 'b. ('a, 'b, Layout.t) Genarray.t -> 'u
    }

    type map_f_t = {
      g : 'a 'b 'c 'd. ('a, 'b, Layout.t) Genarray.t ->
        ('c, 'd, Layout.t) Genarray.t
    }

    type restricted_map_f_t = {
      h : 'a 'b. ('a, 'b, Layout.t) Genarray.t ->
        ('a, 'b, Layout.t) Genarray.t
    }

    (** [apply f data] applies the functions wrapped in [f] to [data]. *)
    let apply f data =
      match data with
      | Int8 x -> f.f x
      | UInt8 x -> f.f x
      | Int16 x -> f.f x
      | UInt16 x -> f.f x
      | Int32 x -> f.f x
      | Float32 x -> f.f x
      | Float64 x -> f.f x

    (** [map f data] applies the functions wrapped in [f] to [data]. *)
    let map f data =
      match data with
      | Int8 x -> f.g x
      | UInt8 x -> f.g x
      | Int16 x -> f.g x
      | UInt16 x -> f.g x
      | Int32 x -> f.g x
      | Float32 x -> f.g x
      | Float64 x -> f.g x

    (** [map_restricted f data] applies the functions wrapped in [f] to
        [data]. *)
    let map_restricted f data =
      match data with
      | Int8 x -> Int8 (f.h x)
      | UInt8 x -> UInt8 (f.h x)
      | Int16 x -> Int16 (f.h x)
      | UInt16 x -> UInt16 (f.h x)
      | Int32 x -> Int32 (f.h x)
      | Float32 x -> Float32 (f.h x)
      | Float64 x -> Float64 (f.h x)

    (** [dims data] returns an array holding the dimensions of [data]. *)
    let dims data = apply { f = Genarray.dims } data

    (** [size_of_element data] returns the size in bytes of a single element
        from [data]. *)
    let size_of_element data = apply { f = Genarray.size_of_element } data

    (** [sub data initial_index length] returns a subsection of [data] of length
        [length] starting from [initial_index]. *)
    let sub data initial_index length =
      let h x = Layout.sub x initial_index length in
      map_restricted { h } data

    (** [reshape data dims] works in exactly the same way as Bigarray.reshape *)
    let reshape data dims =
      let h x = reshape x dims in
      map_restricted { h } data

    (** [slice data new_dimensions] returns a subarray of [data] with dimensions
        [new_dimensions]. *)
    let slice data new_dimensions =
      let h x = Layout.slice x new_dimensions in
      map_restricted { h } data

    (** [blit source dest] copies the data from [source] to [dest]. *)
    let blit source dest =
      let f x y = Genarray.blit x y in
      match (source, dest) with
      | (Int8 x, Int8 y) -> f x y
      | (UInt8 x, UInt8 y) -> f x y
      | (Int16 x, Int16 y) -> f x y
      | (UInt16 x, UInt16 y) -> f x y
      | (Int32 x, Int32 y) -> f x y
      | (Float32 x, Float32 y) -> f x y
      | (Float64 x, Float64 y) -> f x y
      | _ -> invalid_arg "blit"

    (** [get_* data which_datum] returns a single element from [data] at
        the location specified by the array [which_datum]. *)
    let get_int data which_datum =
      let f x = Genarray.get x which_datum in
      match data with
      | Int8 x -> f x
      | UInt8 x -> f x
      | Int16 x -> f x
      | UInt16 x -> f x
      | _ -> invalid_arg "get_int"

    let get_int32 data which_datum =
      let f x = Genarray.get x which_datum in
      match data with
      | Int32 x -> f x
      | _ -> invalid_arg "get_int32"

    let get_float data which_datum =
      let f x = Genarray.get x which_datum in
      match data with
      | Float32 x -> f x
      | Float64 x -> f x
      | _ -> invalid_arg "get_float"

    (** [set_* data which_datum value] sets a single element of [data] at
        the location specified by the array [which_datum] to [value]. *)
    let set_int data which_datum value =
      let f x = Genarray.set x which_datum value in
      match data with
      | Int8 x -> f x
      | UInt8 x -> f x
      | Int16 x -> f x
      | UInt16 x -> f x
      | _ -> invalid_arg "set_int"

    let set_int32 data which_datum value =
      let f x = Genarray.set x which_datum value in
      match data with
      | Int32 x -> f x
      | _ -> invalid_arg "set_int32"

    let set_float data which_datum value =
      let f x = Genarray.set x which_datum value in
      match data with
      | Float32 x -> f x
      | Float64 x -> f x
      | _ -> invalid_arg "set_float"

    (** {6 Convenience, Array-module-like functions} *)

    (** Number of elements in the whole data set. *)
    let elems b =
      let dims = dims b in
      Array.fold_left ( * ) 1 dims

    (** [apply_* f b] applies [f] to every element of [b], modifying [b] in
        place. *)
    let apply_int f data =
      match data with
      | Int8 x -> Genarray.modify f x
      | UInt8 x -> Genarray.modify f x
      | Int16 x -> Genarray.modify f x
      | UInt16 x -> Genarray.modify f x
      | _ -> invalid_arg "apply_int"

    let apply_int32 f data =
      match data with
      | Int32 x -> Genarray.modify f x
      | _ -> invalid_arg "apply_int32"

    let apply_float f data =
      match data with
      | Float32 x -> Genarray.modify f x
      | Float64 x -> Genarray.modify f x
      | _ -> invalid_arg "apply_float"

    (** [map_* f kind b] applies [f] to every element of [b], returning the
        resulting values.  [b] is not changed. *)
    let map_int f kind data =
      match data with
      | Int8 x -> Genarray.map f kind x
      | UInt8 x -> Genarray.map f kind x
      | Int16 x -> Genarray.map f kind x
      | UInt16 x -> Genarray.map f kind x
      | _ -> invalid_arg "map_int"

    let map_int32 f kind data =
      match data with
      | Int32 x -> Genarray.map f kind x
      | _ -> invalid_arg "map_int32"

    let map_float f kind data =
      match data with
      | Float32 x -> Genarray.map f kind x
      | Float64 x -> Genarray.map f kind x
      | _ -> invalid_arg "map_float"

    (** [fold_* f initial b]
        Like Array.fold_left and List.fold_left, but over an entire data set.
        Note: The Bigarray is flattened, so this function works over all of
        the elements, regardless of the Bigarray dimensions. *)
    let fold_int f initial data =
      match data with
      | Int8 x -> Genarray.fold f initial x
      | UInt8 x -> Genarray.fold f initial x
      | Int16 x -> Genarray.fold f initial x
      | UInt16 x -> Genarray.fold f initial x
      | _ -> invalid_arg "fold_int"

    let fold_int32 f initial data =
      match data with
      | Int32 x -> Genarray.fold f initial x
      | _ -> invalid_arg "fold_int32"

    let fold_float f initial data =
      match data with
      | Float32 x -> Genarray.fold f initial x
      | Float64 x -> Genarray.fold f initial x
      | _ -> invalid_arg "fold_float"
  end

  type hdf_vdata_pack_action_t =
    | HDF_VSPACK
    | HDF_VSUNPACK

  (** Pack and unpack Vdata fields.  This is outside of the low-level module
      because is relies on defintions in the the {!Hdf4} module. *)
  external vs_fpack : int32 -> hdf_vdata_pack_action_t -> string ->
    (int, Bigarray.int8_unsigned_elt, Layout.t) Bigarray.Genarray.t ->
    int -> int -> string -> Hdf4.t array -> unit
    = "ml_VSfpack_bytecode" "ml_VSfpack"

  module Attribute = struct
    type t = Hdf4.t

    (** [to_* a] convert [a] to an appropriately typed OCaml array. *)
    let to_int a =
      Array1.to_array (array1_of_genarray (Hdf4.map_int identity int a))
    let to_int32 a =
      Array1.to_array (array1_of_genarray (Hdf4.map_int32 identity int32 a))
    let to_float a =
      Array1.to_array (array1_of_genarray (Hdf4.map_float identity float64 a))

    (** [get_* a] gets the first value from [a].  Mostly useful when [a] is an
        attribute with only a single value. *)
    let get_int a = Hdf4.get_int a [|0|]
    let get_int32 a = Hdf4.get_int32 a [|0|]
    let get_float a = Hdf4.get_float a [|0|]
  end

  (** {6 HDF4 SDS interface} *)
  module Sd = struct
    open Hdf4

    let check_interface i =
      match i.sdid with
      | None -> invalid_arg "Uninitialized interface"
      | Some x -> x

    type fill_value_t =
      | Int_fill of int
      | Float_fill of float
      | Int32_fill of int32

    type t = {
      (* The SDS entry name *)
      name : string;
      (* The actual SDS contents *)
      data : Hdf4.t;
      (* Attributes associated with this SDS *)
      attributes : Hdf4.t Smap.t;
      (* Fill value for missing or unset values *)
      fill : fill_value_t option;
      (* What kind of data are these? *)
      data_type : Hdf4.data_t;
    }

    (** [make ?attributes ?fill name data] creates a {!t} from the given
        information. *)
    let make ?(attributes = Smap.empty) ?fill name data =
      {
        name = name;
        data = data;
        attributes = attributes;
        fill = fill;
        data_type = _data_type_from_t data;
      }

    (** [select ?name ?index interface] will give the sds_id of the given
        [name] or [index] associated with the SD [interface].
        Raises [Invalid_argument "select"] if both or neither [name] and
        [index] are provided. *)
    let select ?name ?index interface =
      let sdid = check_interface interface in
      let i =
        match name, index with
        | None, None
        | Some _, Some _ -> raise (Invalid_argument "select")
        | Some n, None -> sd_nametoindex sdid n
        | None, Some i -> Int32.of_int i
      in
      sd_select sdid i

    (** [info sds_id] can be used when a SDS is already selected. *)
    let info sds_id =
      let (sds_name, rank, dimsizes, data_type, num_attrs) =
        sd_getinfo sds_id
      in
      let dims =
        Array.init (Int32.to_int rank) (fun i -> Int32.to_int dimsizes.(i))
      in
      (
        sds_name,
        dims,
        hdf_datatype_to_mlvariant data_type,
        Int32.to_int num_attrs
      )

    let wrap_sds_call f ?name ?index interface =
      try_finally
        (select ?name ?index interface)
        sd_endaccess
        f

    let read_fill ?name ?index interface =
      let f g = wrap_sds_call g ?name ?index interface in
      function
        | Hdf4.Int8 _
        | Hdf4.UInt8 _
        | Hdf4.Int16 _
        | Hdf4.UInt16 _ -> Int_fill (f sd_getfillvalue_int)
        | Hdf4.Int32 _ -> Int32_fill (f sd_getfillvalue_int32)
        | Hdf4.Float32 _
        | Hdf4.Float64 _ -> Float_fill (f sd_getfillvalue_float)

    let write_fill sds_id =
      function
        | Int_fill x -> sd_setfillvalue_int sds_id x
        | Int32_fill x -> sd_setfillvalue_int32 sds_id x
        | Float_fill x -> sd_setfillvalue_float sds_id x

    let read_attributes sds_id =
      let (_, _, _, num_attrs) = info sds_id in
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
          apply { f } data;
          name, data
      )
      |> Array.enum
      |> Smap.of_enum

    let write_attributes sds_id attrs =
      Smap.iter (
        fun name data ->
          let f x =
            sd_setattr sds_id name
              (Hdf4.mlvariant_to_hdf_datatype (Hdf4._data_type_from_t data))
              (Int32.of_int (Hdf4.elems data))
              x
          in
          apply { f } data
      ) attrs

    (** [read_ga ?name ?index ?subset kind interface] -
        Must provide ONE of [name] OR [index].
        A subset of an SDS can be read by passing a list of [(offset, length)]
        pairs for each dimension in the data set.  An [offset] of [0] would
        start from the first element of that dimension of the SDS.  If no [dim]
        list is provided then all data are read and returned.
        It returns a Bigarray containing the SDS contents. *)
    let read_ga ?name ?index ?subset kind interface =
      wrap_sds_call ?name ?index (
        fun sds_id ->
          let (_, dims, _, _) = info sds_id in
          (* Default to reading the entire data set.  The "stride" option is
             set to NULL in the C stubs. *)
          let start, edges =
            let start_default = Array.make (Array.length dims) 0 in
            let edges_default = dims in
            match subset with
            | None -> (start_default, edges_default)
            | Some sub ->
                let sub' =
                  List.mapi (fun i x -> x |? (start_default.(i), dims.(i))) sub
                in
                Array.of_list (List.map fst sub'),
                Array.of_list (List.map snd sub')
          in
          (* Make sure the correct number of dimensions were provided *)
          let () =
            if
              Array.length start <> Array.length dims ||
              Array.length edges <> Array.length dims
            then
              raise (Invalid_argument "wrong number of dimensions")
            else
              ()
          in
          let ba = Genarray.create kind Layout.layout edges in
          let start = Array.map Int32.of_int start in
          let edges = Array.map Int32.of_int edges in
          sd_readdata sds_id start edges ba;
          ba
      ) interface

    (** [read ?name ?index ?subset interface] -
        Must provide ONE of [name] OR [index].  Returns the SDS contents and
        related metadata. *)
    let read ?name ?index ?subset interface =
      let (sds_name, _, data_type, _) =
        wrap_sds_call info ?name ?index interface
      in
      let data =
        let f k = read_ga ?name ?index ?subset k interface in
        match data_type with
        | `int8 -> Int8 (f int8_signed)
        | `uint8 -> UInt8 (f int8_unsigned)
        | `int16 -> Int16 (f int16_signed)
        | `uint16 -> UInt16 (f int16_unsigned)
        | `int32 -> Int32 (f int32)
        | `float32 -> Float32 (f float32)
        | `float64 -> Float64 (f float64)
      in
      let attributes = wrap_sds_call read_attributes ?name ?index interface in
      let fill =
        unless (Failure "Error getting SDS fill value.")
          (fun i -> read_fill ?name ?index i data)
          interface
      in
      {
        name = sds_name;
        data;
        attributes;
        fill;
        data_type;
      }

    (** [read_all interface] returns a {!Smap.t} of the SDS contents of
        [interface]. *)
    let read_all interface =
      let sdid = check_interface interface in
      let (num_sds, _) = sd_fileinfo sdid in
      let sds =
        Array.init
          (Int32.to_int num_sds)
          (fun i -> read ~index:i interface)
      in
      Array.enum sds
      |> Enum.map (fun s -> s.name, s)
      |> Smap.of_enum

    let create interface data =
      let sdid = check_interface interface in
      match
        sd_create sdid data.name
          (Hdf4.mlvariant_to_hdf_datatype data.data_type)
          (Array.map Int32.of_int (Hdf4.dims data.data))
      with
      | (-1l) ->
          let error_str = he_string (he_value 0l) in
          raise (Hdf4.HdfError error_str)
      | sds_id -> sds_id

    (** [unpack sds kind] returns a function which can be used to unpack the
        data in [sds] IF it has the appropriate attributes set
        (["scale_factor"], ["add_offset"], ["missing_value"].  Any numbers
        matching [missing_value] map to [nan], while all other value are
        altered according to [scale_factor] and [add_offset].

        Raises [Invalid_argument] if any of the required attributes are
        missing. *)
    let unpack sds kind =
      let scale_factor =
        Smap.find "scale_factor" sds.attributes
        |> Option.map Attribute.get_float
      in
      let offset =
        Smap.find "add_offset" sds.attributes
        |> Option.map Attribute.get_float
      in
      let missing =
        Smap.find "missing_value" sds.attributes
        |> Option.map Attribute.get_int
      in
      match scale_factor, offset, missing with
      | Some s, Some o, Some m ->
          Hdf4.map_int
            (fun x -> if x = m then nan else float_of_int x *. s +. o)
            kind
            sds.data
      | _ ->
          raise (Invalid_argument "Unable to find attributes for unpacking")

    (** [write_data sds_id data] creates an entry and writes [data] to the file
        referenced by [sds_id]. *)
    let write_data sds_id data =
      let f x = sd_writedata sds_id x in
      match data with
      | Int8 x -> f x
      | UInt8 x -> f x
      | Int16 x -> f x
      | UInt16 x -> f x
      | Int32 x -> f x
      | Float32 x -> f x
      | Float64 x -> f x

    let write interface data =
      try_finally
        (create interface data)
        sd_endaccess
        (
          fun sds_id ->
            (* Write the data, attributes and fill value (if there is one). *)
            write_data sds_id data.data;
            write_attributes sds_id data.attributes;
            Option.may (write_fill sds_id) data.fill;
        )
  end

  module Vdata =
  struct
    open Hdf4

    let check_interface i =
      match i.fid with
      | None -> invalid_arg "Uninitialized interface"
      | Some x -> x

    module Field = struct
      type t = {
        name : string;
        order : int;
        data : Hdf4.t;
        attributes : Hdf4.t Smap.t;
      }
    end

    type t = {
      name : string;
      fields : Field.t Smap.t;
      attributes : Hdf4.t Smap.t;
      vdata_class : string;
    }

    (** [n_field_elements data] returns the number of elements in the fields
        in [data].  If all of the fields in [data] are not the same length
        then [Invalid_argument "Vdata field element count mismatch"] is
        raised. *)
    let n_field_elements fields =
      let n_elements =
        Smap.values fields
        |> Enum.map (fun f -> Hdf4.elems f.Field.data)
        |> List.of_enum
      in
      match n_elements with
      | [] -> invalid_arg "Vdata has no fields"
      | hd :: tl ->
          if List.for_all (( = ) hd) tl then (
            hd
          )
          else (
            (* All fields in a Vdata entry need to have the same number of
               elements *)
            invalid_arg "Vdata field element count mismatch"
          )

    (** All functions in this module will raise
        [Invalid_argument "Vdata is an attribute"] if one attempts to read a
        Vdata marked as being an attribute without using the proper attribute
        reading and writing functions. *)
    let check_attribute vdata_id =
      if vs_isattr vdata_id = 1 then
        raise (Invalid_argument "Vdata is an attribute")
      else
        ()

    (** [read_attributes ?field vdata_id] reads attribute(s) associated with a
        particular Vdata or Vdata field.  If [field] is provide (and not None)
        then attributes for that field will be returned.  Otherwise, attributes
        for the Vdata are returned. *)
    let read_attributes ?field vdata_id =
      (* If no field is provided, get the number of attributes for the vdata
         itself. *)
      let field = field |? -1l in
      let num_attrs = vs_fnattrs vdata_id field in
      let attributes =
        Array.init num_attrs (
          fun i ->
            let (name, data_type, num_values, data_size) =
              vs_attrinfo vdata_id field i
            in
            let data =
              Hdf4.create (Hdf4.hdf_datatype_to_mlvariant data_type)
                [|Int32.to_int num_values|]
            in
            let f x =
              vs_getattr vdata_id (Int32.to_int field) (Int32.of_int i) x
            in
            (* This actually loads the data *)
            apply { f } data;
            name, data
        )
      in
      Array.enum attributes
      |> Smap.of_enum

    (** [write_attributes ?field vdata_id attrs] adds the data in [attrs] as
        atributes for the Vdata entry associated with [vdata_id]. *)
    let write_attributes ?field vdata_id attrs =
      let field = Int32.of_int (field |? -1) in
      Smap.iter (
        fun name data ->
          let f x =
            vs_setattr vdata_id field name
              (Hdf4.mlvariant_to_hdf_datatype
                (Hdf4._data_type_from_t data))
              (Int32.of_int (Hdf4.elems data))
              x
          in
          apply { f } data;
      ) attrs

    (** Tests to see if the given Vdata or Vgroup class or name is a reserved
        name. *)
    external is_reserved_name : string -> bool = "ml_is_reserved_name"

    let is_special vdata_id =
      let vdata_name = vs_getname vdata_id in
      let vdata_class = vs_getclass vdata_id in
      vdata_class <> "" && (
        is_reserved_name vdata_class ||
        is_reserved_name vdata_name
      )

    (** [make_field ?init vdata_id name] allocates a {!Field.t} appropriate
        for the field named [name] from [vdata_id].  Attributes are read, but
        not the Vdata itself unless [init] is [true]. *)
    let make_field ?(init = false) vdata_id name =
      let index = vs_findex vdata_id name in
      let attributes = read_attributes ~field:index vdata_id in
      let n_elements = vs_elts vdata_id in
      let order = Int32.to_int (vf_fieldorder vdata_id index) in
      let data =
        Hdf4.create
          (Hdf4._data_type_from_hdf_type (vf_fieldtype vdata_id index))
          [|Int32.to_int n_elements * order|]
      in
      if init then (
        vs_setfields vdata_id name;
        let f x = vs_read vdata_id x n_elements HDF_FULL_INTERLACE in
        match data with
        | Int8 x -> f x
        | UInt8 x -> f x
        | Int16 x -> f x
        | UInt16 x -> f x
        | Int32 x -> f x
        | Float32 x -> f x
        | Float64 x -> f x
      );
      {
        Field.name;
        order;
        data;
        attributes;
      }

    (** [read_fields vdata_id] will return all of the fields associated with the
        given [vdata_id]. *)
    let read_fields vdata_id =
      (* Make sure we're not trying to read an attribute. *)
      check_attribute vdata_id;
      let _, field_names = vs_getfields vdata_id in
      (* Allocate and read everything but the data for the fields *)
      let fields =
        let names =
          Pcre.asplit ~pat:"," field_names
        in
        Array.map (make_field vdata_id) names
      in
      (* Extract just the field data *)
      let field_data = Array.map (fun f -> f.Field.data) fields in
      (* Total size, in bytes, of the Vdata fields *)
      let n_records = vs_elts vdata_id in
      let total_bytes =
        Int32.to_int (
          Int32.mul
            (vs_sizeof vdata_id field_names)
            n_records
        )
      in

      (* The data will be read in to this buffer, then "unpacked" in to the
         field arrays. *)
      let data_buffer =
        Genarray.create int8_unsigned Layout.layout [|total_bytes|]
      in
      vs_read vdata_id data_buffer n_records HDF_FULL_INTERLACE;
      vs_fpack vdata_id HDF_VSUNPACK
        field_names data_buffer total_bytes (Int32.to_int n_records)
        field_names field_data;
      Array.enum fields
      |> Enum.map Field.(fun f -> f.name, f)
      |> Smap.of_enum

    let read_by_id vdata_id =
      (* This check is needed to make sure that we are not attempting to read some
         sort of HDF metadata. *)
      if is_special vdata_id then (
        None
      )
      else (
        Some {
          name = vs_getname vdata_id;
          fields = read_fields vdata_id;
          attributes = read_attributes vdata_id;
          vdata_class = vs_getclass vdata_id;
        }
      )

    (** [ref_from_index interface index] gets the Vdata ref associated with the
        [index]'th Vdata entry. *)
    let ref_from_index interface index =
      let fid = check_interface interface in
      let rec f i vdata_ref =
        let new_ref = vs_getid fid vdata_ref in
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

    (** [read ?name ?index interface] reads the Vdata identified by [name] or by
        [index]. *)
    let read ?name ?index interface =
      let fid = check_interface interface in
      let vdata_id =
        let vdata_ref =
          match name, index with
          | None, None
          | Some _, Some _ ->
              raise (Invalid_argument "Vdata.read needs a name OR an index")
          | Some n, None -> vs_find fid n
          | None, Some i -> ref_from_index interface i
        in
        vs_attach fid vdata_ref "r"
      in
      let vdata = read_by_id vdata_id in
      (* Detach when we are done reading. *)
      vs_detach vdata_id;
      match vdata with
      | None -> raise (Invalid_argument "Vdata.read_data")
      | Some vd -> vd

    (** [map f interface] will go through each Vdata in [interface],
        applying [f] and returning an array of the results.  This is mainly
        meant to be used in the [read_all] function below but may have other
        uses. *)
    let map f interface =
      let fid = check_interface interface in
      let rec loop accu vdata_ref =
        match vs_attach fid vdata_ref "r" with
        | -1l -> List.rev accu
        | vdata_id ->
            let result = f vdata_id in
            vs_detach vdata_id;
            loop (result :: accu) (vs_getid fid vdata_ref)
      in
      Array.of_list (loop [] (vs_getid fid (-1l)))

    (** [read_all interface] will return an array of Vdata, one for each
        Vdata in [interface].  The data in the returned array are in the same
        order as those in [interface]. *)
    let read_all interface =
      Array.filter_map identity (map read_by_id interface)
      |> Array.enum
      |> Enum.map (fun v -> v.name, v)
      |> Smap.of_enum

    (** For internal use.  This packs multiple fields in to one lump of bytes,
        ready for ingestion by vs_write. *)
    let pack_fields vdata_id fields =
      (* Extract just the field data *)
      let field_data =
        Enum.map (fun f -> f.Field.data) (Smap.values fields)
        |> Array.of_enum
      in
      (* Total size, in bytes, of the Vdata fields *)
      let total_bytes =
        let byte_sizes =
          Enum.map (
            fun field ->
              Hdf4.elems field.Field.data *
              Hdf4.size_of_element field.Field.data
              (* No need to multiply by the order or n_records as that information
                 is carried in the number of elements in the data. *)
          ) (Smap.values fields)
        in
        Enum.reduce ( + ) byte_sizes
      in

      let field_names =
        let names = Enum.map (fun f -> f.Field.name) (Smap.values fields) in
        String.concat "," (List.of_enum names)
      in

      let n_records = n_field_elements fields in

      (* Pack all of the data in to a single, HDF-ready buffer *)
      let data_buffer =
        Genarray.create int8_unsigned Layout.layout [|total_bytes|]
      in
      vs_fpack vdata_id HDF_VSPACK
        field_names data_buffer total_bytes n_records
        field_names field_data;
      data_buffer

    (** [write interface data] will write out the Vdata given in [data] to a new
        Vdata entry in [interface]. *)
    let write interface data =
      let fid = check_interface interface in
      let vdata_id = vs_attach fid (-1l) "w" in
      (* Make a Vdata-ready, comma-delimited string of the field names *)
      let field_names =
        let names = List.of_enum (Smap.keys data.fields) in
        String.concat "," names
      in

      (* Define each field in the Vdata *)
      Enum.iteri (
        fun i (name, field) ->
          let field_type = Hdf4._hdf_type_from_t field.Field.data in
          (* Define the field *)
          vs_fdefine vdata_id
            field.Field.name field_type (Int32.of_int field.Field.order);
          (* Write out the attributes for this field *)
          write_attributes ~field:i vdata_id field.Field.attributes;
      ) (Smap.enum data.fields);

      (* Set some metadata *)
      vs_setname vdata_id data.name;
      vs_setclass vdata_id data.vdata_class;
      vs_setfields vdata_id field_names;

      (* Write the main Vdata *)
      let n_elements = n_field_elements data.fields in
      let packed_fields = pack_fields vdata_id data.fields in
      vs_write vdata_id packed_fields (Int32.of_int n_elements)
        HDF_FULL_INTERLACE;

      (* Write the Vdata attributes *)
      write_attributes vdata_id data.attributes;

      (* All done *)
      vs_detach vdata_id;
      ()
  end

  (*
  module Vgroup = struct
    (** Example:
    Vgroup [|
      Vgroup [|
        SDS (Hdf4.create `int8 [|10|])
      |];
      SDS (Hdf4.create `int8 [|10|]);
    |]
    *)
    type vgroup_t =
      | Vgroup of vgroup_t array
      | SDS of SD.Generic.t
      | Vdata of Vdata.t

    (** TODO -- Just about everything.  Vgroup reading, writing and manipulating
        code will end up in here eventually. *)
  end
  *)
end

(** An easy/ready-to-use batch of modules *)

module Hdf4_map = struct
  include Map.StringMap
  include Exceptionless
  include Infix
end

module Hdf4c = struct
  module Map = Hdf4_map
  module H = Make(C_layout)(Hdf4_map)
  include H
  include Hdf4
end

module Hdf4f = struct
  module Map = Hdf4_map
  module H = Make(Fortran_layout)(Hdf4_map)
  include H
  include Hdf4
end

module Easy = struct
  module Hdf4c = Hdf4c
  module Hdf4f = Hdf4f
end

