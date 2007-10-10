(** {5 HDF4} *)

module G = ExtBigarray

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

exception BadDataType of string * string
(** Exception raised if we try to get at data which we shouldn't or can't handle. *)

(** {6 Internal library functions } *)

external is_hdf: string -> bool = "ml_Hishdf"
(** Check to see if a file is a valid HDF4 data file.
    [is_hdf filename] give true if [filename] is a HDF4 file, false if not.
*)

let create (data_type : data_t) dimensions =
	let f x = Bigarray.Genarray.create x Bigarray.c_layout dimensions in
	match data_type with
		| `INT8 -> Int8 (f Bigarray.int8_signed)
		| `UINT8 -> UInt8 (f Bigarray.int8_unsigned)
		| `INT16 -> Int16 (f Bigarray.int16_signed)
		| `UINT16 -> UInt16 (f Bigarray.int16_unsigned)
		| `INT32 -> Int32 (f Bigarray.int32)
		| `FLOAT32 -> Float32 (f Bigarray.float32)
		| `FLOAT64 -> Float64 (f Bigarray.float64)
(** Create a bigarray of the appropriate type. *)

let ( _string_from_data_type : data_t -> string ) = function
	| `INT8 -> "INT8"
	| `UINT8 -> "UINT8"
	| `INT16 -> "INT16"
	| `UINT16 -> "UINT16"
	| `INT32 -> "INT32"
	| `FLOAT32 -> "FLOAT32"
	| `FLOAT64 -> "FLOAT64"
(** Get a string representation for a given data type. *)

let ( _type_size_in_bytes : data_t -> int ) = function
	| `INT8
	| `UINT8 -> 1
	| `INT16
	| `UINT16 -> 2
	| `INT32 -> 4
	| `FLOAT32 -> 4
	| `FLOAT64 -> 8
(** Get a string representation for a given data type. *)

let ( _data_type_from_string : string -> data_t ) = function
	| "INT8" -> `INT8
	| "UINT8" -> `UINT8
	| "INT16" -> `INT16
	| "UINT16" -> `UINT16
	| "INT32" -> `INT32
	| "FLOAT32" -> `FLOAT32
	| "FLOAT64" -> `FLOAT64
	| x -> raise (BadDataType ("Invalid or unhandled data type ", x))
(** String to variant type tage. *)

let ( _data_type_from_t : t -> data_t ) = function
	| Int8 _ -> `INT8
	| UInt8 _ -> `UINT8
	| Int16 _ -> `INT16
	| UInt16 _ -> `UINT16
	| Int32 _ -> `INT32
	| Float32 _ -> `FLOAT32
	| Float64 _ -> `FLOAT64
(** Get a variant type tag from a Hdf.t *)

let dims data =
	let f x = Bigarray.Genarray.dims x in
	match data with
		| Int8 x -> f x
		| UInt8 x -> f x
		| Int16 x -> f x
		| UInt16 x -> f x
		| Int32 x -> f x
		| Float32 x -> f x
		| Float64 x -> f x
(** [dims data] returns an array holding the dimensions of [data]. *)

let sub data initial_index length =
	let f x = Bigarray.Genarray.sub_left x initial_index length in
	match data with
		| Int8 x -> Int8 (f x)
		| UInt8 x -> UInt8 (f x)
		| Int16 x -> Int16 (f x)
		| UInt16 x -> UInt16 (f x)
		| Int32 x -> Int32 (f x)
		| Float32 x -> Float32 (f x)
		| Float64 x -> Float64 (f x)
(** [sub data initial_index length] returns a subsection of [data] of length
    [length] starting from [initial_index].
*)

let reshape data dims =
	let f x = Bigarray.reshape x dims in
	match data with
		| Int8 x -> Int8 (f x)
		| UInt8 x -> UInt8 (f x)
		| Int16 x -> Int16 (f x)
		| UInt16 x -> UInt16 (f x)
		| Int32 x -> Int32 (f x)
		| Float32 x -> Float32 (f x)
		| Float64 x -> Float64 (f x)
(** [reshape data dims] works in exactly the same way as Bigarray.reshape *)

let slice data new_dimensions =
	let f x = Bigarray.Genarray.slice_left x new_dimensions in
	match data with
		| Int8 x -> Int8 (f x)
		| UInt8 x -> UInt8 (f x)
		| Int16 x -> Int16 (f x)
		| UInt16 x -> UInt16 (f x)
		| Int32 x -> Int32 (f x)
		| Float32 x -> Float32 (f x)
		| Float64 x -> Float64 (f x)
(** [slice data new_dimensions] returns a subarray of [data] with dimensions
    [new_dimensions].
*)

let blit source dest =
	let f x y = Bigarray.Genarray.blit x y in
	match (source, dest) with
		| (Int8 x, Int8 y) -> f x y
		| (UInt8 x, UInt8 y) -> f x y
		| (Int16 x, Int16 y) -> f x y
		| (UInt16 x, UInt16 y) -> f x y
		| (Int32 x, Int32 y) -> f x y
		| (Float32 x, Float32 y) -> f x y
		| (Float64 x, Float64 y) -> f x y
		| _ -> raise (BadDataType ("blit", ""))
(** [blit source dest] copies the data from [source] to [dest].
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
(** [get_* data which_datum] returns a single element from [data] at
    the location specified by the array [which_datum].
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
(** [set_* data which_datum value] sets a single element of [data] at
    the location specified by the array [which_datum] to [value].
*)

(** Convenience, Array-module-like functions. *)

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
	type interface_id = InterfaceID of int
	type data_id = DataID of int
	type data_spec = {
		index : data_id;
		name : string;
		data_type : data_t;
		dimensions : int array;
	}
  (** This type holds the basic specs for SDS entries in a HDF4 file. *)

	external c_open_file: string -> int = "ml_SDstart"

	let open_file filename = InterfaceID (c_open_file filename)
  (** Open the Scientific Data Set (SDS) data interface for a HDF4 file.
      [open_file filename] returns the sd_id for [filenames]'s interface.
      This function opens the SDS interface in read-only mode.
  *)

	external c_get_info: int -> int -> (int * string * string * int array * int) = "ml_SDgetinfo"
  (** Get specs for an individual SDS entry.
      [get_sd_info sd_id sd_index] gives [(sd_index, SDS name, data type string,
      array of dimensions, num_attrs)]
  *)

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
  (** [get_info interface_id data_id] - Get specs for the given SDS entry. *)

	external c_get_data: int -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
		string = "ml_SDreaddata"
  (** [c_get_data sd_id sd_index genarray] returns a string with the data type.
      The actual data is stored in [genarray].
  *)

	let get_data (InterfaceID sd_id) (DataID sd_index) data =
		c_get_data sd_id sd_index data
  (** [get_data sd_id sd_index genarray] returns a string with the data type.
      The actual data is stored in [genarray].
  *)

	external c_close_file: int -> int = "ml_SDend"

	let close_file (InterfaceID cs_id) = c_close_file cs_id
  (** [close_file sd_id] closes the given SDS interface.
      The return value indicates success or failure.  But this will change to
      a return value of unit and the function will then throw an exception when
      there is an error.
  *)

  (** {6 Pure OCaml functions and wrappers}
      These functions make a prettier, and hopefully more functional, interface to
      underlying C library calls.
  *)

	let get_spec_list sd_id =
		let rec f i =
			try
				let info = get_info sd_id (DataID i) in
				info :: f (i + 1)
			with
          (* TODO - This isn't a very good error checking method. *)
				Failure x ->
					if i > 0 then
						[]
					else
						failwith x
		in
		f 0
  (** [get_spec_list sd_id] returns a list of the information given by {!Hdf.get_sd_info}
      for each SDS available through the given [sd_id] interface.
  *)

	let get_index_from_name sd_id name =
		(List.find (fun x -> x.name = name) (get_spec_list sd_id)).index
  (** [get_sd_index_from_name sd_id name] will return the index of the SDS named [name]. *)

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
  (** Actually GET the data in to the bigarray. *)

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
  (** [get_by_name interface_id data_name] returns the data called [data_name]. *)

	external c_open_sd_create: string -> int = "ml_SDstart_create"

	let open_sd_create filename = InterfaceID (c_open_sd_create filename)
  (** [open_sd_create filename] creates a new HDF4 file [filename] with an SDS interface.
  *)

	external c_create: int -> string -> string -> int ->
		('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> int = "ml_SDcreate"
  (** [c_create sd_id name datatype_string number_of_dimensions genarray_of_dimensions] *)

	let create (InterfaceID sd_id) name data_type dimensions =
		let dims =
			Bigarray.genarray_of_array1
			(Bigarray.Array1.of_array Bigarray.int Bigarray.c_layout dimensions)
		in
		let data_type_string = _string_from_data_type data_type in
		DataID (c_create sd_id name data_type_string (Array.length dimensions) dims)
  (** [create sd_id name data_type dimensions] creates an entry in [sd_id] named
      [name] of type [data_type] with the given [dimensions].
  *)

	external c_write_data: int -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> unit = "ml_SDwritedata"

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
  (** [write_data sds_id data] creates an entry and writes [data] to the file
      referenced by [sd_id].
  *)

	let create_data sd_id name data =
		let sds_id = create sd_id name (_data_type_from_t data) (dims data) in
		write_data sds_id data
  (** [create_data sd_id name data] - create and write an SDS named [name] *)

	external c_end_access: int -> int = "ml_SDendaccess"

	let end_access (DataID sds_id) = c_end_access sds_id
  (** [end_access sds_id]
      See above comments about return values...
  *)
end

module Vdata =
struct
	type interface_id = InterfaceID of int32
	type data_id = DataID of int32

	type data_spec = {
		index : data_id;
		name : string;
		field_name_list : string;
		data_size : int; (* Size (in bytes) of each record. *)
		records : int;   (* Number of records. *)
	}
  (** This type holds the basic specs for SDS entries in a HDF4 file. *)

  (** {6 Native C interface}
      The following are the raw calls to the C functions.  Most, if not all, should
      be wrapped in appropriate OCaml goodness. *)

	external c_open_hdf: string -> int32 = "ml_Hopen"

	let open_hdf filename = InterfaceID (c_open_hdf filename)
  (** [open_hdf filename] returns the file_id for the Vdata interface of the
      HDF4 file [filename].
      This opens the file in read-only mode.
  *)

	external c_close_hdf: int32 -> int = "ml_Hclose"

	let close_hdf (InterfaceID file_id) = c_close_hdf file_id
  (** [close_hdf file_id] closes the given HDF4 interface referenced by file_id.
      See {!Hdf.close_sd} for a discussion on the return value.
  *)

	external c_start: int32 -> int32 = "ml_Vstart"

	let start (InterfaceID file_id) = DataID (c_start file_id)
  (** [start file_id] gives the Vdata index number for the given HDF interface.
  *)

	external c_get_info: int32 -> int32 -> (int32 * string * string * int * int) = "ml_VSinquire"
  (** [c_get_info file_id vd_index] retrieves information on the data referenced
      by vd_index.
      The return value is [(vd_index, name, field name list, data size, number of records)]
  *)

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
  (** [get_info file_id vd_index] *)

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
  (** [get_spec_list file_id] returns a list of the information given by {!Hdf.get_vs_info}
      for each Vdata available through the given [file_id] interface.
  *)

	external c_get_data: int32 -> int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
		string = "ml_VSread"
  (** [get_VDdata file_id vd_index genarray] returns the data type string for [vd_index].
      The data referenced by [vd_index] is stored in [genarray].
  *)

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
  (** Actually GET the data in to the bigarray. *)        

	external c_vd_index_from_vdata_ref: int32 -> int32 -> int32 = "ml_vd_index_from_vdata_ref"

	let vd_index_from_vdata_ref (InterfaceID file_id) (DataID vdata_ref) =
		DataID (c_vd_index_from_vdata_ref file_id vdata_ref)
  (** Get the index from a vdata_ref value. *)

	external c_VSfind: int32 -> string -> int32 = "ml_VSfind"

	let get_index_from_name (InterfaceID file_id) name =
    (* (List.find (fun x -> x.name = name) (get_spec_list file_id)).index *)
    (* DataID (c_get_index_from_name file_id name) *)
		vd_index_from_vdata_ref (InterfaceID file_id) (DataID (c_VSfind file_id name))
  (** [get_sd_index_from_name sd_id name] will return the index of the SDS named [name]. *)

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
  (** [get_by_name interface_id data_name] returns the data called [data_name]. *)

	external c_v_end: int32 -> int = "ml_Vend"

	let v_end (InterfaceID file_id) = c_v_end file_id
  (** [v_end file_id] closes the Vdata interface to [file_id].
      See {!Hdf.close_sd} for a discussion on the return value.
  *)

  (** {6 File and data set creation functions} *)

	external open_hdf_create: string -> int = "ml_Hopen_create"
  (** [open_hdf_create filename] creates the HDF4 file [filename].
      [filename] must exist.
      File is opened read-only.
      See above comments about return values...
  *)

	external c_open_hdf_rw: string -> int32 = "ml_Hopen_rw"

	let open_hdf_rw filename = InterfaceID (c_open_hdf_rw filename)
  (** Like {!Hdf.open_hdf_create} but file is opened read-write. *)

  (** NOTE: [vdata_id] values must be [Int32] values, as the HDF library uses the full 32 range. *)

	external c_create: int32 -> int32 = "ml_VSattach_create"

	let create (InterfaceID file_id) = DataID (c_create file_id)
  (** [create file_id] returns a vdata_id for a new Vdata entry in the HDF4 file
      referenced by [file_id].
  *)

	external c_define_fields: int32 -> string -> string -> int -> unit = "ml_VSfdefine"

	let define_fields (DataID vdata_id) fieldname data_type order =
		c_define_fields vdata_id fieldname (_string_from_data_type data_type) order
  (** [define_fields vdata_id fieldname datatype_string order] *)

	external c_set_fields: int32 -> string -> unit = "ml_VSsetfields"

	let set_fields (DataID vdata_id) field_list_string = c_set_fields vdata_id field_list_string
  (** [set_fields vdata_id field_list_string] *)

	external c_write: int32 -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> int -> unit = "ml_VSwrite"

	let write (DataID vdata_id) data number_of_records =
		let f x = c_write vdata_id x number_of_records in
		match data with
				Int8 x -> f x
			| UInt8 x -> f x
			| Int16 x -> f x
			| UInt16 x -> f x
			| Int32 x -> f x
			| Float32 x -> f x
			| Float64 x -> f x
  (** [write vdata_id data number_of_records] *)

	external c_set_name: int32 -> string -> unit = "ml_VSsetname"

	let set_name (DataID vdata_id) name = c_set_name vdata_id name
  (** [set_name vdata_id name] *)

	external c_set_class: int32 -> string -> unit = "ml_VSsetclass"

	let set_class (DataID vdata_id) name = c_set_class vdata_id name
  (** [set_class vdata_id class] *)

	external c_detach: int32 -> unit = "ml_VSdetach"

	let detach (DataID vdata_id) = c_detach vdata_id
  (** [vs_detach vdata_id]
      Detach the Vdata interface.
  *)
end