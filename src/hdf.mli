module type HDF4_LAYOUT_TYPE =
  sig
    type t
    val layout : t Bigarray.layout
    val sub :
      ('a, 'b, t) Bigarray.Genarray.t ->
      int -> int -> ('a, 'b, t) Bigarray.Genarray.t
    val slice :
      ('a, 'b, t) Bigarray.Genarray.t ->
      int array -> ('a, 'b, t) Bigarray.Genarray.t
  end
module C_layout : HDF4_LAYOUT_TYPE with type t = Bigarray.c_layout
module Fortran_layout : HDF4_LAYOUT_TYPE with type t = Bigarray.fortran_layout
module Hdf4_low_level :
  sig
    type _fcd = char option
    and char8 = char
    and uchar8 = char
    and hdf_int8 = char
    and hdf_uint8 = char
    and hdf_int16 = int
    and hdf_uint16 = int
    and intn = int
    and uintn = int
    and float32 = float
    and float64 = float
    and hDF_RESULT = intn
    and enum_1 =
      Hdf_wrapper.enum_1 =
        DFNT_FLOAT32
      | DFNT_FLOAT
      | DFNT_FLOAT64
      | DFNT_DOUBLE
      | DFNT_FLOAT128
      | DFNT_INT8
      | DFNT_UINT8
      | DFNT_INT16
      | DFNT_UINT16
      | DFNT_INT32
      | DFNT_UINT32
      | DFNT_INT64
      | DFNT_UINT64
      | DFNT_INT128
      | DFNT_UINT128
      | DFNT_UCHAR8
      | DFNT_UCHAR
      | DFNT_CHAR8
      | DFNT_CHAR
      | DFNT_CHAR16
      | DFNT_UCHAR16
      | DFNT_NFLOAT32
      | DFNT_NFLOAT64
      | DFNT_NFLOAT128
      | DFNT_NINT8
      | DFNT_NUINT8
      | DFNT_NINT16
      | DFNT_NUINT16
      | DFNT_NINT32
      | DFNT_NUINT32
      | DFNT_NINT64
      | DFNT_NUINT64
      | DFNT_NINT128
      | DFNT_NUINT128
      | DFNT_NCHAR8
      | DFNT_NCHAR
      | DFNT_NUCHAR8
      | DFNT_NUCHAR
      | DFNT_NCHAR16
      | DFNT_NUCHAR16
      | DFNT_LFLOAT32
      | DFNT_LFLOAT64
      | DFNT_LFLOAT128
      | DFNT_LINT8
      | DFNT_LUINT8
      | DFNT_LINT16
      | DFNT_LUINT16
      | DFNT_LINT32
      | DFNT_LUINT32
      | DFNT_LINT64
      | DFNT_LUINT64
      | DFNT_LINT128
      | DFNT_LUINT128
      | DFNT_LCHAR8
      | DFNT_LCHAR
      | DFNT_LUCHAR8
      | DFNT_LUCHAR
      | DFNT_LCHAR16
      | DFNT_LUCHAR16
    and hdf_data_type = enum_1
    and enum_2 = Hdf_wrapper.enum_2 = DFACC_READ | DFACC_WRITE | DFACC_RDWR | DFACC_CREATE
    and access_type = enum_2
    and enum_3 =
      Hdf_wrapper.enum_3 =
        AN_UNDEF
      | AN_DATA_LABEL
      | AN_DATA_DESC
      | AN_FILE_LABEL
      | AN_FILE_DESC
    and ann_type = enum_3
    and hFILEID = int32
    and enum_4 = Hdf_wrapper.enum_4 = COMP_MODEL_STDIO
    and comp_model_t = enum_4
    and enum_5 =
      Hdf_wrapper.enum_5 =
        COMP_CODE_NONE
      | COMP_CODE_RLE
      | COMP_CODE_NBIT
      | COMP_CODE_SKPHUFF
      | COMP_CODE_DEFLATE
      | COMP_CODE_SZIP
      | COMP_CODE_INVALID
      | COMP_CODE_JPEG
    and comp_coder_t = enum_5
    and enum_6 =
      Hdf_wrapper.enum_6 =
        DFE_NONE
      | DFE_FNF
      | DFE_DENIED
      | DFE_ALROPEN
      | DFE_TOOMANY
      | DFE_BADNAME
      | DFE_BADACC
      | DFE_BADOPEN
      | DFE_NOTOPEN
      | DFE_CANTCLOSE
      | DFE_READERROR
      | DFE_WRITEERROR
      | DFE_SEEKERROR
      | DFE_RDONLY
      | DFE_BADSEEK
      | DFE_PUTELEM
      | DFE_GETELEM
      | DFE_CANTLINK
      | DFE_CANTSYNC
      | DFE_BADGROUP
      | DFE_GROUPSETUP
      | DFE_PUTGROUP
      | DFE_GROUPWRITE
      | DFE_DFNULL
      | DFE_ILLTYPE
      | DFE_BADDDLIST
      | DFE_NOTDFFILE
      | DFE_SEEDTWICE
      | DFE_NOSUCHTAG
      | DFE_NOFREEDD
      | DFE_BADTAG
      | DFE_BADREF
      | DFE_NOMATCH
      | DFE_NOTINSET
      | DFE_BADOFFSET
      | DFE_CORRUPT
      | DFE_NOREF
      | DFE_DUPDD
      | DFE_CANTMOD
      | DFE_DIFFFILES
      | DFE_BADAID
      | DFE_OPENAID
      | DFE_CANTFLUSH
      | DFE_CANTUPDATE
      | DFE_CANTHASH
      | DFE_CANTDELDD
      | DFE_CANTDELHASH
      | DFE_CANTACCESS
      | DFE_CANTENDACCESS
      | DFE_TABLEFULL
      | DFE_NOTINTABLE
      | DFE_UNSUPPORTED
      | DFE_NOSPACE
      | DFE_BADCALL
      | DFE_BADPTR
      | DFE_BADLEN
      | DFE_NOTENOUGH
      | DFE_NOVALS
      | DFE_ARGS
      | DFE_INTERNAL
      | DFE_NORESET
      | DFE_GENAPP
      | DFE_UNINIT
      | DFE_CANTINIT
      | DFE_CANTSHUTDOWN
      | DFE_BADDIM
      | DFE_BADFP
      | DFE_BADDATATYPE
      | DFE_BADMCTYPE
      | DFE_BADNUMTYPE
      | DFE_BADORDER
      | DFE_RANGE
      | DFE_BADCONV
      | DFE_BADTYPE
      | DFE_BADSCHEME
      | DFE_BADMODEL
      | DFE_BADCODER
      | DFE_MODEL
      | DFE_CODER
      | DFE_CINIT
      | DFE_CDECODE
      | DFE_CENCODE
      | DFE_CTERM
      | DFE_CSEEK
      | DFE_MINIT
      | DFE_COMPINFO
      | DFE_CANTCOMP
      | DFE_CANTDECOMP
      | DFE_NOENCODER
      | DFE_NODIM
      | DFE_BADRIG
      | DFE_RINOTFOUND
      | DFE_BADATTR
      | DFE_LUTNOTFOUND
      | DFE_BADTABLE
      | DFE_BADSDG
      | DFE_BADNDG
      | DFE_VGSIZE
      | DFE_VTAB
      | DFE_CANTADDELEM
      | DFE_BADVGNAME
      | DFE_BADVGCLASS
      | DFE_BADFIELDS
      | DFE_NOVS
      | DFE_SYMSIZE
      | DFE_BADATTACH
      | DFE_BADVSNAME
      | DFE_BADVSCLASS
      | DFE_VSWRITE
      | DFE_VSREAD
      | DFE_BADVH
      | DFE_FIELDSSET
      | DFE_VSCANTCREATE
      | DFE_VGCANTCREATE
      | DFE_CANTATTACH
      | DFE_CANTDETACH
      | DFE_BITREAD
      | DFE_BITWRITE
      | DFE_BITSEEK
      | DFE_TBBTINS
      | DFE_BVNEW
      | DFE_BVSET
      | DFE_BVGET
      | DFE_BVFIND
    and hdf_err_code_t = enum_6
    and struct_8 = intn
    and struct_9 = intn
    and struct_7 =
      Hdf_wrapper.struct_7 = {
      skphuff : struct_8;
      deflate : struct_9;
    }
    and comp_info = struct_7
    and gr_interlace_t = hdf_int16
    and enum_10 =
      Hdf_wrapper.enum_10 =
        NOT_SDAPI_ID
      | SD_ID
      | SDS_ID
      | DIM_ID
    and hdf_idtype_t = enum_10
    external h_open : string -> access_type -> hdf_int16 -> int32
      = "camlidl_hdf_wrapper_Hopen"
    external h_close : int32 -> unit = "camlidl_hdf_wrapper_Hclose"
    external h_ishdf : string -> intn = "camlidl_hdf_wrapper_Hishdf"
    external he_string : hdf_err_code_t -> string
      = "camlidl_hdf_wrapper_HEstring"
    external he_value : int32 -> hdf_err_code_t
      = "camlidl_hdf_wrapper_HEvalue"
    external hep_clear : unit -> unit = "camlidl_hdf_wrapper_HEPclear"
    external v_nattrs : int32 -> intn = "camlidl_hdf_wrapper_Vnattrs"
    external v_findattr : int32 -> string -> intn
      = "camlidl_hdf_wrapper_Vfindattr"
    external v_attrinfo :
      int32 -> intn -> intn * string * int32 * int32 * int32
      = "camlidl_hdf_wrapper_Vattrinfo"
    external v_getversion : int32 -> int32
      = "camlidl_hdf_wrapper_Vgetversion"
    external vs_findex : int32 -> string -> int32
      = "camlidl_hdf_wrapper_VSfindex"
    external vs_nattrs : int32 -> intn = "camlidl_hdf_wrapper_VSnattrs"
    external vs_fnattrs : int32 -> int32 -> intn
      = "camlidl_hdf_wrapper_VSfnattrs"
    external vs_findattr : int32 -> int32 -> string -> intn
      = "camlidl_hdf_wrapper_VSfindattr"
    external vs_attrinfo :
      int32 -> int32 -> intn -> string * int32 * int32 * int32
      = "camlidl_hdf_wrapper_VSattrinfo"
    external vs_isattr : int32 -> intn = "camlidl_hdf_wrapper_VSisattr"
    external vicheckcompat : hFILEID -> int32
      = "camlidl_hdf_wrapper_vicheckcompat"
    external vimakecompat : hFILEID -> int32
      = "camlidl_hdf_wrapper_vimakecompat"
    external vcheckcompat : string -> int32
      = "camlidl_hdf_wrapper_vcheckcompat"
    external vmakecompat : string -> int32
      = "camlidl_hdf_wrapper_vmakecompat"
    external vs_elts : int32 -> int32 = "camlidl_hdf_wrapper_VSelts"
    external vs_getinterlace : int32 -> int32
      = "camlidl_hdf_wrapper_VSgetinterlace"
    external vs_setinterlace : int32 -> int32 -> unit
      = "camlidl_hdf_wrapper_VSsetinterlace"
    external vs_getfields : int32 -> int32 * string
      = "camlidl_hdf_wrapper_VSgetfields"
    external vs_fexist : int32 -> string -> intn
      = "camlidl_hdf_wrapper_VSfexist"
    external vs_sizeof : int32 -> string -> int32
      = "camlidl_hdf_wrapper_VSsizeof"
    external vs_dump : int32 -> unit = "camlidl_hdf_wrapper_VSdump"
    external vs_setname : int32 -> string -> unit
      = "camlidl_hdf_wrapper_VSsetname"
    external vs_setclass : int32 -> string -> unit
      = "camlidl_hdf_wrapper_VSsetclass"
    external vs_getname : int32 -> string = "camlidl_hdf_wrapper_VSgetname"
    external vs_getclass : int32 -> string = "camlidl_hdf_wrapper_VSgetclass"
    external vs_inquire : int32 -> int32 * int32 * string * int32 * string
      = "camlidl_hdf_wrapper_VSinquire"
    external vs_lone : hFILEID -> int32 option -> int32 -> int32
      = "camlidl_hdf_wrapper_VSlone"
    external v_lone : hFILEID -> int32 option -> int32 -> int32
      = "camlidl_hdf_wrapper_Vlone"
    external v_find : hFILEID -> string -> int32
      = "camlidl_hdf_wrapper_Vfind"
    external vs_find : hFILEID -> string -> int32
      = "camlidl_hdf_wrapper_VSfind"
    external v_findclass : hFILEID -> string -> int32
      = "camlidl_hdf_wrapper_Vfindclass"
    external vs_findclass : hFILEID -> string -> int32
      = "camlidl_hdf_wrapper_VSfindclass"
    external vs_setblocksize : int32 -> int32 -> intn
      = "camlidl_hdf_wrapper_VSsetblocksize"
    external vs_setnumblocks : int32 -> int32 -> intn
      = "camlidl_hdf_wrapper_VSsetnumblocks"
    external vs_getblockinfo : int32 -> intn * int32 * int32
      = "camlidl_hdf_wrapper_VSgetblockinfo"
    external v_setzap : unit -> unit = "camlidl_hdf_wrapper_Vsetzap"
    external v_initialize : hFILEID -> unit
      = "camlidl_hdf_wrapper_Vinitialize"
    external v_finish : hFILEID -> unit = "camlidl_hdf_wrapper_Vfinish"
    external v_open : string -> intn -> hdf_int16 -> hFILEID
      = "camlidl_hdf_wrapper_Vopen"
    external v_close : hFILEID -> intn = "camlidl_hdf_wrapper_Vclose"
    external v_attach : hFILEID -> int32 -> string -> int32
      = "camlidl_hdf_wrapper_Vattach"
    external v_detach : int32 -> int32 = "camlidl_hdf_wrapper_Vdetach"
    external v_insert : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_Vinsert"
    external v_setname : int32 -> string -> int32
      = "camlidl_hdf_wrapper_Vsetname"
    external v_setclass : int32 -> string -> int32
      = "camlidl_hdf_wrapper_Vsetclass"
    external v_getid : hFILEID -> int32 -> int32
      = "camlidl_hdf_wrapper_Vgetid"
    external v_getnext : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_Vgetnext"
    external v_getname : int32 -> int32 * string
      = "camlidl_hdf_wrapper_Vgetname"
    external v_getclass : int32 -> int32 * string
      = "camlidl_hdf_wrapper_Vgetclass"
    external v_inquire : int32 -> intn * int32 * string
      = "camlidl_hdf_wrapper_Vinquire"
    external v_delete : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_Vdelete"
    external v_deletetagref : int32 -> int32 -> int32 -> intn
      = "camlidl_hdf_wrapper_Vdeletetagref"
    external vs_attach : hFILEID -> int32 -> string -> int32
      = "camlidl_hdf_wrapper_VSattach"
    external vs_detach : int32 -> unit = "camlidl_hdf_wrapper_VSdetach"
    external vsq_uerytag : int32 -> int32 = "camlidl_hdf_wrapper_VSQuerytag"
    external vsq_ueryref : int32 -> int32 = "camlidl_hdf_wrapper_VSQueryref"
    external vs_getid : hFILEID -> int32 -> int32
      = "camlidl_hdf_wrapper_VSgetid"
    external vs_getversion : int32 -> int32
      = "camlidl_hdf_wrapper_VSgetversion"
    external vs_delete : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_VSdelete"
    external vs_appendable : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_VSappendable"
    external vs_setfields : int32 -> string -> unit
      = "camlidl_hdf_wrapper_VSsetfields"
    external vs_fdefine : int32 -> string -> hdf_data_type -> int32 -> unit
      = "camlidl_hdf_wrapper_VSfdefine"
    external vf_nfields : int32 -> int32 = "camlidl_hdf_wrapper_VFnfields"
    external vf_fieldname : int32 -> int32 -> string
      = "camlidl_hdf_wrapper_VFfieldname"
    external vf_fieldtype : int32 -> int32 -> hdf_data_type
      = "camlidl_hdf_wrapper_VFfieldtype"
    external vf_fieldisize : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_VFfieldisize"
    external vf_fieldesize : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_VFfieldesize"
    external vf_fieldorder : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_VFfieldorder"
    external vs_setexternalfile : int32 -> string -> int32 -> intn
      = "camlidl_hdf_wrapper_VSsetexternalfile"
    external vsp_shutdown : unit -> intn = "camlidl_hdf_wrapper_VSPshutdown"
    external vs_seek : int32 -> int32 -> int32 = "camlidl_hdf_wrapper_VSseek"
    external sd_start : string -> access_type -> int32
      = "camlidl_hdf_wrapper_SDstart"
    external sd_end : int32 -> unit = "camlidl_hdf_wrapper_SDend"
    external sd_fileinfo : int32 -> int32 * int32
      = "camlidl_hdf_wrapper_SDfileinfo"
    external sd_select : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_SDselect"
    external sd_nametoindex : int32 -> string -> int32
      = "camlidl_hdf_wrapper_SDnametoindex"
    external sd_create : int32 -> string -> int32 -> int32 array -> int32
      = "camlidl_hdf_wrapper_SDcreate"
    external sd_getdimid : int32 -> intn -> int32
      = "camlidl_hdf_wrapper_SDgetdimid"
    external sd_setdimname : int32 -> string -> intn
      = "camlidl_hdf_wrapper_SDsetdimname"
    external sd_endaccess : int32 -> unit = "camlidl_hdf_wrapper_SDendaccess"
    external sd_attrinfo : int32 -> int32 -> string * int32 * int32
      = "camlidl_hdf_wrapper_SDattrinfo"
    external sd_setdatastrs :
      int32 -> string -> string -> string -> string -> unit
      = "camlidl_hdf_wrapper_SDsetdatastrs"
    external sd_setcal :
      int32 -> float64 -> float64 -> float64 -> float64 -> int32 -> intn
      = "camlidl_hdf_wrapper_SDsetcal_bytecode"
      "camlidl_hdf_wrapper_SDsetcal"
    external sd_setfillmode : int32 -> intn -> intn
      = "camlidl_hdf_wrapper_SDsetfillmode"
    external sd_getdatastrs :
      int32 -> intn -> string * string * string * string
      = "camlidl_hdf_wrapper_SDgetdatastrs"
    external sd_getcal :
      int32 -> intn * float64 * float64 * float64 * float64 * int32
      = "camlidl_hdf_wrapper_SDgetcal"
    external sd_setdimstrs : int32 -> string -> string -> string -> intn
      = "camlidl_hdf_wrapper_SDsetdimstrs"
    external sd_diminfo : int32 -> string * int32 * int32 * int32
      = "camlidl_hdf_wrapper_SDdiminfo"
    external sd_getdimstrs : int32 -> intn -> intn * string * string * string
      = "camlidl_hdf_wrapper_SDgetdimstrs"
    external sd_setexternalfile : int32 -> string -> int32 -> intn
      = "camlidl_hdf_wrapper_SDsetexternalfile"
    external sd_setnbitdataset :
      int32 -> intn -> intn -> intn -> intn -> intn
      = "camlidl_hdf_wrapper_SDsetnbitdataset"
    external sd_setcompress : int32 -> comp_coder_t -> comp_info -> unit
      = "camlidl_hdf_wrapper_SDsetcompress"
    external sd_getcompress : int32 -> comp_coder_t * comp_info
      = "camlidl_hdf_wrapper_SDgetcompress"
    external sd_findattr : int32 -> string -> int32
      = "camlidl_hdf_wrapper_SDfindattr"
    external sd_idtoref : int32 -> int32 = "camlidl_hdf_wrapper_SDidtoref"
    external sd_reftoindex : int32 -> int32 -> int32
      = "camlidl_hdf_wrapper_SDreftoindex"
    external sd_isrecord : int32 -> int32 = "camlidl_hdf_wrapper_SDisrecord"
    external sd_iscoordvar : int32 -> intn
      = "camlidl_hdf_wrapper_SDiscoordvar"
    external sd_setaccesstype : int32 -> uintn -> intn
      = "camlidl_hdf_wrapper_SDsetaccesstype"
    external sd_setblocksize : int32 -> int32 -> intn
      = "camlidl_hdf_wrapper_SDsetblocksize"
    external sd_setdimval_comp : int32 -> intn -> intn
      = "camlidl_hdf_wrapper_SDsetdimval_comp"
    external sd_isdimval_bwcomp : int32 -> intn
      = "camlidl_hdf_wrapper_SDisdimval_bwcomp"
    external sd_checkempty : int32 -> intn option -> int32
      = "camlidl_hdf_wrapper_SDcheckempty"
    external sd_idtype : int32 -> hdf_idtype_t
      = "camlidl_hdf_wrapper_SDidtype"
    external sd_setchunkcache : int32 -> int32 -> int32 -> intn
      = "camlidl_hdf_wrapper_SDsetchunkcache"
    val hdf_open : string -> access_type -> hdf_int16 -> int32
    val hdf_close : int32 -> unit
    val v_start : hFILEID -> unit
    val v_end : hFILEID -> unit
    val he_clear : unit -> unit
    type hdf_vdata_interlace_t = HDF_NO_INTERLACE | HDF_FULL_INTERLACE
    external vs_write :
      int32 ->
      ('a, 'b, 'c) Bigarray.Genarray.t ->
      int32 -> hdf_vdata_interlace_t -> unit = "ml_VSwrite"
    external vs_read :
      int32 ->
      ('a, 'b, 'c) Bigarray.Genarray.t ->
      int32 -> hdf_vdata_interlace_t -> unit = "ml_VSread"
    external sd_writedata :
      int32 -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
      = "ml_SDwritedata"
    external sd_readdata :
      int32 ->
      int32 array ->
      int32 array -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
      = "ml_SDreaddata"
    external sd_readattr :
      int32 -> int32 -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
      = "ml_SDreadattr"
    external sd_setattr :
      int32 ->
      string ->
      int32 -> int32 -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
      = "ml_SDsetattr"
    external vs_getattr :
      int32 ->
      int -> int32 -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
      = "ml_VSgetattr"
    external vs_setattr :
      int32 ->
      int32 ->
      string ->
      int32 -> int32 -> ('a, 'b, 'c) Bigarray.Genarray.t -> unit
      = "ml_VSsetattr_bytecode" "ml_VSsetattr"
    external sd_getfillvalue_float : int32 -> float
      = "ml_SDgetfillvalue_float"
    external sd_getfillvalue_int : int32 -> int = "ml_SDgetfillvalue_int"
    external sd_getfillvalue_int32 : int32 -> int32
      = "ml_SDgetfillvalue_int32"
    external sd_setfillvalue_float : int32 -> float -> unit
      = "ml_SDsetfillvalue_float"
    external sd_setfillvalue_int : int32 -> int -> unit
      = "ml_SDsetfillvalue_int"
    external sd_setfillvalue_int32 : int32 -> int32 -> unit
      = "ml_SDsetfillvalue_int32"
    val sd_getinfo : int32 -> string * int32 * int32 array * int32 * int32
  end
module type MAPPABLE = sig
  type key
  type 'a t

  val empty : 'a t
  val enum : 'a t -> (key * 'a) Batteries.Enum.t
  val of_enum : (key * 'a) Batteries.Enum.t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val keys : 'a t -> key Batteries.Enum.t
  val values : 'a t -> 'a Batteries.Enum.t
  val find : key -> 'a t -> 'a option
end
module Make :
  functor (Layout : HDF4_LAYOUT_TYPE) ->
    functor (Smap : MAPPABLE with type key = string) ->
      sig
        module Hdf4 :
          sig
            type data_t =
                [ `float32
                | `float64
                | `int16
                | `int32
                | `int8
                | `uint16
                | `uint8 ]
            exception HdfError of string
            type interface = private {
              sdid : int32 option;
              fid : int32 option;
            }
            val open_file :
              ?access:Hdf4_low_level.access_type -> string -> interface
            val close_file : interface -> unit
            val open_file_in :
              ?access:Hdf4_low_level.access_type ->
              (interface -> 'a) -> string -> 'a
            type t =
                Int8 of
                  (int, Bigarray.int8_signed_elt, Layout.t)
                  Bigarray.Genarray.t
              | UInt8 of
                  (int, Bigarray.int8_unsigned_elt, Layout.t)
                  Bigarray.Genarray.t
              | Int16 of
                  (int, Bigarray.int16_signed_elt, Layout.t)
                  Bigarray.Genarray.t
              | UInt16 of
                  (int, Bigarray.int16_unsigned_elt, Layout.t)
                  Bigarray.Genarray.t
              | Int32 of
                  (int32, Bigarray.int32_elt, Layout.t)
                  Bigarray.Genarray.t
              | Float32 of
                  (float, Bigarray.float32_elt, Layout.t)
                  Bigarray.Genarray.t
              | Float64 of
                  (float, Bigarray.float64_elt, Layout.t)
                  Bigarray.Genarray.t
            val create : data_t -> int array -> t
            val to_int8 :
              t ->
              (int, Bigarray.int8_signed_elt, Layout.t)
              Bigarray.Genarray.t
            val to_uint8 :
              t ->
              (int, Bigarray.int8_unsigned_elt, Layout.t)
              Bigarray.Genarray.t
            val to_int16 :
              t ->
              (int, Bigarray.int16_signed_elt, Layout.t)
              Bigarray.Genarray.t
            val to_uint16 :
              t ->
              (int, Bigarray.int16_unsigned_elt, Layout.t)
              Bigarray.Genarray.t
            val to_int32 :
              t ->
              (int32, Bigarray.int32_elt, Layout.t)
              Bigarray.Genarray.t
            val to_float32 :
              t ->
              (float, Bigarray.float32_elt, Layout.t)
              Bigarray.Genarray.t
            val to_float64 :
              t ->
              (float, Bigarray.float64_elt, Layout.t)
              Bigarray.Genarray.t
            val of_int8 :
              (int, Bigarray.int8_signed_elt, Layout.t)
              Bigarray.Genarray.t -> t
            val of_uint8 :
              (int, Bigarray.int8_unsigned_elt, Layout.t)
              Bigarray.Genarray.t -> t
            val of_int16 :
              (int, Bigarray.int16_signed_elt, Layout.t)
              Bigarray.Genarray.t -> t
            val of_uint16 :
              (int, Bigarray.int16_unsigned_elt, Layout.t)
              Bigarray.Genarray.t -> t
            val of_int32 :
              (int32, Bigarray.int32_elt, Layout.t)
              Bigarray.Genarray.t -> t
            val of_float32 :
              (float, Bigarray.float32_elt, Layout.t)
              Bigarray.Genarray.t -> t
            val of_float64 :
              (float, Bigarray.float64_elt, Layout.t)
              Bigarray.Genarray.t -> t
            val is_hdf : string -> bool
            external hdf_datatype_to_mlvariant : int32 -> data_t
              = "hdf_datatype_to_mlvariant"
            external mlvariant_to_hdf_datatype : data_t -> int32
              = "mlvariant_to_hdf_datatype"
            val _type_size_in_bytes : data_t -> int
            val _data_type_from_t : t -> data_t
            val _data_type_from_hdf_type :
              Hdf4_low_level.hdf_data_type -> data_t
            val _hdf_type_from_t : t -> Hdf4_low_level.enum_1
            val dims : t -> int array
            val size_of_element : t -> int
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
              t -> ('a, 'b, Layout.t) Bigarray.Genarray.t
            val map_int32 :
              (int32 -> 'a) ->
              ('a, 'b) Bigarray.kind ->
              t -> ('a, 'b, Layout.t) Bigarray.Genarray.t
            val map_float :
              (float -> 'a) ->
              ('a, 'b) Bigarray.kind ->
              t -> ('a, 'b, Layout.t) Bigarray.Genarray.t
            val fold_int : ('a -> int -> 'a) -> 'a -> t -> 'a
            val fold_int32 : ('a -> int32 -> 'a) -> 'a -> t -> 'a
            val fold_float : ('a -> float -> 'a) -> 'a -> t -> 'a

            val to_int_array : t -> int array
            val to_int32_array : t -> int32 array
            val to_float_array : t -> float array
            (** [to_*_array a] converts [a] to an appropriately typed OCaml
                array. *)

            val to_string : t -> string
            val of_string : string -> t
            (** Convert strings to/from {!t} values.
                These functions use [char_of_int] and [int_of_char] internally
                so they will only work with {!t} values containing integers
                and only with ASCII characters. *)

            val get_one_int : t -> int
            val get_one_int32 : t -> int32
            val get_one_float : t -> float
            (** [get_one_* a] gets the first value from [a].  Mostly useful
                when [a] is an attribute with only a single value. *)
          end
        type hdf_vdata_pack_action_t = HDF_VSPACK | HDF_VSUNPACK
        external vs_fpack :
          int32 ->
          hdf_vdata_pack_action_t ->
          string ->
          (int, Bigarray.int8_unsigned_elt, Layout.t)
          Bigarray.Genarray.t ->
          int -> int -> string -> Hdf4.t array -> unit = "ml_VSfpack_bytecode"
          "ml_VSfpack"
        module Sd :
          sig
            type fill_value_t =
                Int_fill of int
              | Float_fill of float
              | Int32_fill of int32
            type t = {
              name : string;
              data : Hdf4.t;
              attributes : Hdf4.t Smap.t;
              fill : fill_value_t option;
              data_type : Hdf4.data_t;
            }
            val make :
              ?attributes:Hdf4.t Smap.t ->
              ?fill:fill_value_t -> string -> Hdf4.t -> t
            val select : ?name:string -> ?index:int -> Hdf4.interface -> int32
            val info : int32 -> string * int array * Hdf4.data_t * int
            val wrap_sds_call :
              (int32 -> 'a) ->
              ?name:string -> ?index:int -> Hdf4.interface -> 'a
            val read_fill :
              ?name:string ->
              ?index:int -> Hdf4.interface -> Hdf4.t -> fill_value_t
            val write_fill : int32 -> fill_value_t -> unit
            val read_attributes : int32 -> Hdf4.t Smap.t
            val write_attributes : int32 -> Hdf4.t Smap.t -> unit
            val read_ga :
              ?name:string ->
              ?index:int ->
              ?subset:(int * int) option list ->
              ('a, 'b) Bigarray.kind ->
              Hdf4.interface -> ('a, 'b, Layout.t) Bigarray.Genarray.t
            val read :
              ?name:string ->
              ?index:int ->
              ?subset:(int * int) option list -> Hdf4.interface -> t
            val read_all : Hdf4.interface -> t Smap.t
            val unpack :
              t ->
              (float, 'a) Bigarray.kind ->
              (float, 'a, Layout.t) Bigarray.Genarray.t
            val create : Hdf4.interface -> t -> int32
            val write_data : int32 -> Hdf4.t -> unit
            val write : Hdf4.interface -> t -> unit
          end
        module Vdata :
          sig
            module Field :
              sig
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
            val check_attribute : int32 -> unit
            val read_attributes : ?field:int32 -> int32 -> Hdf4.t Smap.t
            val write_attributes : ?field:int -> int32 -> Hdf4.t Smap.t -> unit
            external is_reserved_name : string -> bool = "ml_is_reserved_name"
            val is_special : int32 -> bool
            val make_field : ?init:bool -> int32 -> string -> Field.t
            val read_fields : int32 -> Field.t Smap.t
            val read_by_id : int32 -> t option
            val ref_from_index : Hdf4.interface -> int -> int32
            val read : ?name:string -> ?index:int -> Hdf4.interface -> t
            val map : (int32 -> 'a) -> Hdf4.interface -> 'a array
            val read_all : Hdf4.interface -> t Smap.t
            val pack_fields :
              int32 ->
              Field.t Smap.t ->
              (int, Bigarray.int8_unsigned_elt, Layout.t)
              Bigarray.Genarray.t
            val write : Hdf4.interface -> t -> unit
          end
      end

module Hdf4_map : sig
  include Batteries.Map.S with type key = string
  include module type of Exceptionless
  include module type of Infix
end

module Hdf4c : sig
  module Map : module type of Hdf4_map
  include module type of Make(C_layout)(Hdf4_map)
  include module type of Hdf4
  module L : module type of Hdf4_low_level
end

module Hdf4f : sig
  module Map : module type of Hdf4_map
  include module type of Make(Fortran_layout)(Hdf4_map)
  include module type of Hdf4
  module L : module type of Hdf4_low_level
end

module Easy : sig
  module Hdf4c : module type of Hdf4c
  module Hdf4f : module type of Hdf4f
end

