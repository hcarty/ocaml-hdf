(* File generated from hdf_wrapper.idl *)

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
  | DFNT_FLOAT32
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
and enum_2 =
  | DFACC_READ
  | DFACC_WRITE
  | DFACC_RDWR
  | DFACC_CREATE
and access_type = enum_2
and enum_3 =
  | AN_UNDEF
  | AN_DATA_LABEL
  | AN_DATA_DESC
  | AN_FILE_LABEL
  | AN_FILE_DESC
and ann_type = enum_3
and hFILEID = int32
and enum_4 =
  | COMP_MODEL_STDIO
and comp_model_t = enum_4
and enum_5 =
  | COMP_CODE_NONE
  | COMP_CODE_RLE
  | COMP_CODE_NBIT
  | COMP_CODE_SKPHUFF
  | COMP_CODE_DEFLATE
  | COMP_CODE_SZIP
  | COMP_CODE_INVALID
  | COMP_CODE_JPEG
and comp_coder_t = enum_5
and enum_6 =
  | DFE_NONE
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
and struct_7 = {
  skphuff: struct_8;
  deflate: struct_9;
}
and comp_info = struct_7
and gr_interlace_t = hdf_int16
and enum_10 =
  | NOT_SDAPI_ID
  | SD_ID
  | SDS_ID
  | DIM_ID
and hdf_idtype_t = enum_10

external h_open : string -> access_type -> hdf_int16 -> int32
	= "camlidl_hdf_wrapper_Hopen"

external h_close : int32 -> unit
	= "camlidl_hdf_wrapper_Hclose"

external h_ishdf : string -> intn
	= "camlidl_hdf_wrapper_Hishdf"

external he_string : hdf_err_code_t -> string
	= "camlidl_hdf_wrapper_HEstring"

external he_value : int32 -> hdf_err_code_t
	= "camlidl_hdf_wrapper_HEvalue"

external hep_clear : unit -> unit
	= "camlidl_hdf_wrapper_HEPclear"

external v_nattrs : int32 -> intn
	= "camlidl_hdf_wrapper_Vnattrs"

external v_findattr : int32 -> string -> intn
	= "camlidl_hdf_wrapper_Vfindattr"

external v_attrinfo : int32 -> intn -> intn * string * int32 * int32 * int32
	= "camlidl_hdf_wrapper_Vattrinfo"

external v_getversion : int32 -> int32
	= "camlidl_hdf_wrapper_Vgetversion"

external vs_findex : int32 -> string -> int32
	= "camlidl_hdf_wrapper_VSfindex"

external vs_nattrs : int32 -> intn
	= "camlidl_hdf_wrapper_VSnattrs"

external vs_fnattrs : int32 -> int32 -> intn
	= "camlidl_hdf_wrapper_VSfnattrs"

external vs_findattr : int32 -> int32 -> string -> intn
	= "camlidl_hdf_wrapper_VSfindattr"

external vs_attrinfo : int32 -> int32 -> intn -> string * int32 * int32 * int32
	= "camlidl_hdf_wrapper_VSattrinfo"

external vs_isattr : int32 -> intn
	= "camlidl_hdf_wrapper_VSisattr"

external vicheckcompat : hFILEID -> int32
	= "camlidl_hdf_wrapper_vicheckcompat"

external vimakecompat : hFILEID -> int32
	= "camlidl_hdf_wrapper_vimakecompat"

external vcheckcompat : string -> int32
	= "camlidl_hdf_wrapper_vcheckcompat"

external vmakecompat : string -> int32
	= "camlidl_hdf_wrapper_vmakecompat"

external vs_elts : int32 -> int32
	= "camlidl_hdf_wrapper_VSelts"

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

external vs_dump : int32 -> unit
	= "camlidl_hdf_wrapper_VSdump"

external vs_setname : int32 -> string -> unit
	= "camlidl_hdf_wrapper_VSsetname"

external vs_setclass : int32 -> string -> unit
	= "camlidl_hdf_wrapper_VSsetclass"

external vs_getname : int32 -> string
	= "camlidl_hdf_wrapper_VSgetname"

external vs_getclass : int32 -> string
	= "camlidl_hdf_wrapper_VSgetclass"

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

external v_setzap : unit -> unit
	= "camlidl_hdf_wrapper_Vsetzap"

external v_initialize : hFILEID -> unit
	= "camlidl_hdf_wrapper_Vinitialize"

external v_finish : hFILEID -> unit
	= "camlidl_hdf_wrapper_Vfinish"

external v_open : string -> intn -> hdf_int16 -> hFILEID
	= "camlidl_hdf_wrapper_Vopen"

external v_close : hFILEID -> intn
	= "camlidl_hdf_wrapper_Vclose"

external v_attach : hFILEID -> int32 -> string -> int32
	= "camlidl_hdf_wrapper_Vattach"

external v_detach : int32 -> int32
	= "camlidl_hdf_wrapper_Vdetach"

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

external vs_detach : int32 -> unit
	= "camlidl_hdf_wrapper_VSdetach"

external vsq_uerytag : int32 -> int32
	= "camlidl_hdf_wrapper_VSQuerytag"

external vsq_ueryref : int32 -> int32
	= "camlidl_hdf_wrapper_VSQueryref"

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

external vf_nfields : int32 -> int32
	= "camlidl_hdf_wrapper_VFnfields"

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

external vsp_shutdown : unit -> intn
	= "camlidl_hdf_wrapper_VSPshutdown"

external vs_seek : int32 -> int32 -> int32
	= "camlidl_hdf_wrapper_VSseek"

external sd_start : string -> access_type -> int32
	= "camlidl_hdf_wrapper_SDstart"

external sd_end : int32 -> unit
	= "camlidl_hdf_wrapper_SDend"

external sd_fileinfo : int32 -> int32 * int32
	= "camlidl_hdf_wrapper_SDfileinfo"

external sd_select : int32 -> int32 -> int32
	= "camlidl_hdf_wrapper_SDselect"

external sd_getinfo : int32 -> string * int32 * int32 array * int32 * int32
	= "camlidl_hdf_wrapper_SDgetinfo"

external sd_nametoindex : int32 -> string -> int32
	= "camlidl_hdf_wrapper_SDnametoindex"

external sd_create : int32 -> string -> int32 -> int32 array -> int32
	= "camlidl_hdf_wrapper_SDcreate"

external sd_getdimid : int32 -> intn -> int32
	= "camlidl_hdf_wrapper_SDgetdimid"

external sd_setdimname : int32 -> string -> intn
	= "camlidl_hdf_wrapper_SDsetdimname"

external sd_endaccess : int32 -> unit
	= "camlidl_hdf_wrapper_SDendaccess"

external sd_attrinfo : int32 -> int32 -> string * int32 * int32
	= "camlidl_hdf_wrapper_SDattrinfo"

external sd_setdatastrs : int32 -> string -> string -> string -> string -> unit
	= "camlidl_hdf_wrapper_SDsetdatastrs"

external sd_setcal : int32 -> float64 -> float64 -> float64 -> float64 -> int32 -> intn
	= "camlidl_hdf_wrapper_SDsetcal_bytecode" "camlidl_hdf_wrapper_SDsetcal"

external sd_setfillmode : int32 -> intn -> intn
	= "camlidl_hdf_wrapper_SDsetfillmode"

external sd_getdatastrs : int32 -> intn -> string * string * string * string
	= "camlidl_hdf_wrapper_SDgetdatastrs"

external sd_getcal : int32 -> intn * float64 * float64 * float64 * float64 * int32
	= "camlidl_hdf_wrapper_SDgetcal"

external sd_setdimstrs : int32 -> string -> string -> string -> intn
	= "camlidl_hdf_wrapper_SDsetdimstrs"

external sd_diminfo : int32 -> string * int32 * int32 * int32
	= "camlidl_hdf_wrapper_SDdiminfo"

external sd_getdimstrs : int32 -> intn -> intn * string * string * string
	= "camlidl_hdf_wrapper_SDgetdimstrs"

external sd_setexternalfile : int32 -> string -> int32 -> intn
	= "camlidl_hdf_wrapper_SDsetexternalfile"

external sd_setnbitdataset : int32 -> intn -> intn -> intn -> intn -> intn
	= "camlidl_hdf_wrapper_SDsetnbitdataset"

external sd_setcompress : int32 -> comp_coder_t -> comp_info -> unit
	= "camlidl_hdf_wrapper_SDsetcompress"

external sd_getcompress : int32 -> comp_coder_t * comp_info
	= "camlidl_hdf_wrapper_SDgetcompress"

external sd_findattr : int32 -> string -> int32
	= "camlidl_hdf_wrapper_SDfindattr"

external sd_idtoref : int32 -> int32
	= "camlidl_hdf_wrapper_SDidtoref"

external sd_reftoindex : int32 -> int32 -> int32
	= "camlidl_hdf_wrapper_SDreftoindex"

external sd_isrecord : int32 -> int32
	= "camlidl_hdf_wrapper_SDisrecord"

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

