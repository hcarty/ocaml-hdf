/* File generated from hdf_wrapper.idl */

#ifndef _CAMLIDL_HDF_WRAPPER_H
#define _CAMLIDL_HDF_WRAPPER_H

#ifdef __cplusplus
#define _CAMLIDL_EXTERN_C extern "C"
#else
#define _CAMLIDL_EXTERN_C extern
#endif

#ifdef _WIN32
#pragma pack(push,8) /* necessary for COM interfaces */
#endif

typedef char *_fcd;

typedef char char8;

typedef unsigned char uchar8;

typedef char hdf_int8;

typedef unsigned char hdf_uint8;

typedef short hdf_int16;

typedef unsigned short hdf_uint16;

typedef long intn;

typedef unsigned long uintn;

typedef float float32;

typedef double float64;

typedef intn HDF_RESULT;

typedef enum {
DFNT_FLOAT32 = 5,
DFNT_FLOAT = 5,
DFNT_FLOAT64 = 6,
DFNT_DOUBLE = 6,
DFNT_FLOAT128 = 7,
DFNT_INT8 = 20,
DFNT_UINT8 = 21,
DFNT_INT16 = 22,
DFNT_UINT16 = 23,
DFNT_INT32 = 24,
DFNT_UINT32 = 25,
DFNT_INT64 = 26,
DFNT_UINT64 = 27,
DFNT_INT128 = 28,
DFNT_UINT128 = 30,
DFNT_UCHAR8 = 3,
DFNT_UCHAR = 3,
DFNT_CHAR8 = 4,
DFNT_CHAR = 4,
DFNT_CHAR16 = 42,
DFNT_UCHAR16 = 43,
DFNT_NFLOAT32 = (4096 | 5),
DFNT_NFLOAT64 = (4096 | 6),
DFNT_NFLOAT128 = (4096 | 7),
DFNT_NINT8 = (4096 | 20),
DFNT_NUINT8 = (4096 | 21),
DFNT_NINT16 = (4096 | 22),
DFNT_NUINT16 = (4096 | 23),
DFNT_NINT32 = (4096 | 24),
DFNT_NUINT32 = (4096 | 25),
DFNT_NINT64 = (4096 | 26),
DFNT_NUINT64 = (4096 | 27),
DFNT_NINT128 = (4096 | 28),
DFNT_NUINT128 = (4096 | 30),
DFNT_NCHAR8 = (4096 | 4),
DFNT_NCHAR = (4096 | 4),
DFNT_NUCHAR8 = (4096 | 3),
DFNT_NUCHAR = (4096 | 3),
DFNT_NCHAR16 = (4096 | 42),
DFNT_NUCHAR16 = (4096 | 43),
DFNT_LFLOAT32 = (16384 | 5),
DFNT_LFLOAT64 = (16384 | 6),
DFNT_LFLOAT128 = (16384 | 7),
DFNT_LINT8 = (16384 | 20),
DFNT_LUINT8 = (16384 | 21),
DFNT_LINT16 = (16384 | 22),
DFNT_LUINT16 = (16384 | 23),
DFNT_LINT32 = (16384 | 24),
DFNT_LUINT32 = (16384 | 25),
DFNT_LINT64 = (16384 | 26),
DFNT_LUINT64 = (16384 | 27),
DFNT_LINT128 = (16384 | 28),
DFNT_LUINT128 = (16384 | 30),
DFNT_LCHAR8 = (16384 | 4),
DFNT_LCHAR = (16384 | 4),
DFNT_LUCHAR8 = (16384 | 3),
DFNT_LUCHAR = (16384 | 3),
DFNT_LCHAR16 = (16384 | 42),
DFNT_LUCHAR16 = (16384 | 43),
} hdf_data_type;

typedef enum {
DFACC_READ = 1,
DFACC_WRITE = 2,
DFACC_RDWR = 3,
DFACC_CREATE = 4,
} access_type;

typedef enum {
AN_UNDEF = -1,
AN_DATA_LABEL = 0,
AN_DATA_DESC,
AN_FILE_LABEL,
AN_FILE_DESC,
} ann_type;

typedef int HFILEID;

typedef enum {
COMP_MODEL_STDIO = 0,
} comp_model_t;

typedef enum {
COMP_CODE_NONE = 0,
COMP_CODE_RLE,
COMP_CODE_NBIT,
COMP_CODE_SKPHUFF,
COMP_CODE_DEFLATE,
COMP_CODE_SZIP,
COMP_CODE_INVALID,
COMP_CODE_JPEG,
} comp_coder_t;

typedef enum {
DFE_NONE = 0,
DFE_FNF,
DFE_DENIED,
DFE_ALROPEN,
DFE_TOOMANY,
DFE_BADNAME,
DFE_BADACC,
DFE_BADOPEN,
DFE_NOTOPEN,
DFE_CANTCLOSE,
DFE_READERROR,
DFE_WRITEERROR,
DFE_SEEKERROR,
DFE_RDONLY,
DFE_BADSEEK,
DFE_PUTELEM,
DFE_GETELEM,
DFE_CANTLINK,
DFE_CANTSYNC,
DFE_BADGROUP,
DFE_GROUPSETUP,
DFE_PUTGROUP,
DFE_GROUPWRITE,
DFE_DFNULL,
DFE_ILLTYPE,
DFE_BADDDLIST,
DFE_NOTDFFILE,
DFE_SEEDTWICE,
DFE_NOSUCHTAG,
DFE_NOFREEDD,
DFE_BADTAG,
DFE_BADREF,
DFE_NOMATCH,
DFE_NOTINSET,
DFE_BADOFFSET,
DFE_CORRUPT,
DFE_NOREF,
DFE_DUPDD,
DFE_CANTMOD,
DFE_DIFFFILES,
DFE_BADAID,
DFE_OPENAID,
DFE_CANTFLUSH,
DFE_CANTUPDATE,
DFE_CANTHASH,
DFE_CANTDELDD,
DFE_CANTDELHASH,
DFE_CANTACCESS,
DFE_CANTENDACCESS,
DFE_TABLEFULL,
DFE_NOTINTABLE,
DFE_UNSUPPORTED,
DFE_NOSPACE,
DFE_BADCALL,
DFE_BADPTR,
DFE_BADLEN,
DFE_NOTENOUGH,
DFE_NOVALS,
DFE_ARGS,
DFE_INTERNAL,
DFE_NORESET,
DFE_GENAPP,
DFE_UNINIT,
DFE_CANTINIT,
DFE_CANTSHUTDOWN,
DFE_BADDIM,
DFE_BADFP,
DFE_BADDATATYPE,
DFE_BADMCTYPE,
DFE_BADNUMTYPE,
DFE_BADORDER,
DFE_RANGE,
DFE_BADCONV,
DFE_BADTYPE,
DFE_BADSCHEME,
DFE_BADMODEL,
DFE_BADCODER,
DFE_MODEL,
DFE_CODER,
DFE_CINIT,
DFE_CDECODE,
DFE_CENCODE,
DFE_CTERM,
DFE_CSEEK,
DFE_MINIT,
DFE_COMPINFO,
DFE_CANTCOMP,
DFE_CANTDECOMP,
DFE_NOENCODER,
DFE_NODIM,
DFE_BADRIG,
DFE_RINOTFOUND,
DFE_BADATTR,
DFE_LUTNOTFOUND,
DFE_BADTABLE,
DFE_BADSDG,
DFE_BADNDG,
DFE_VGSIZE,
DFE_VTAB,
DFE_CANTADDELEM,
DFE_BADVGNAME,
DFE_BADVGCLASS,
DFE_BADFIELDS,
DFE_NOVS,
DFE_SYMSIZE,
DFE_BADATTACH,
DFE_BADVSNAME,
DFE_BADVSCLASS,
DFE_VSWRITE,
DFE_VSREAD,
DFE_BADVH,
DFE_FIELDSSET,
DFE_VSCANTCREATE,
DFE_VGCANTCREATE,
DFE_CANTATTACH,
DFE_CANTDETACH,
DFE_BITREAD,
DFE_BITWRITE,
DFE_BITSEEK,
DFE_TBBTINS,
DFE_BVNEW,
DFE_BVSET,
DFE_BVGET,
DFE_BVFIND,
} hdf_err_code_t;

typedef struct {
  struct {
    intn skp_size;
  } skphuff;
  struct {
    intn level;
  } deflate;
} comp_info;

_CAMLIDL_EXTERN_C int Hopen(/*in*/ char const *path, /*in*/ access_type acc_mode, /*in*/ hdf_int16 ndds);

_CAMLIDL_EXTERN_C HDF_RESULT Hclose(/*in*/ int file_id);

_CAMLIDL_EXTERN_C intn Hishdf(/*in*/ char const *filename);

_CAMLIDL_EXTERN_C char const *HEstring(/*in*/ hdf_err_code_t error_code);

_CAMLIDL_EXTERN_C hdf_err_code_t HEvalue(/*in*/ int level);

_CAMLIDL_EXTERN_C void HEPclear(void);

_CAMLIDL_EXTERN_C intn Vnattrs(/*in*/ int vgid);

_CAMLIDL_EXTERN_C intn Vfindattr(/*in*/ int vgid, /*in*/ char const *attrname);

_CAMLIDL_EXTERN_C intn Vattrinfo(/*in*/ int vgid, /*in*/ intn attrindex, /*out*/ char *name, /*out*/ int *datatype, /*out*/ int *count, /*out*/ int *size);

_CAMLIDL_EXTERN_C int Vgetversion(/*in*/ int vgid);

_CAMLIDL_EXTERN_C HDF_RESULT VSfindex(/*in*/ int vsid, /*in*/ char const *fieldname, /*out*/ int *fldindex);

_CAMLIDL_EXTERN_C intn VSnattrs(/*in*/ int vsid);

_CAMLIDL_EXTERN_C intn VSfnattrs(/*in*/ int vsid, /*in*/ int findex);

_CAMLIDL_EXTERN_C intn VSfindattr(/*in*/ int vsid, /*in*/ int findex, /*in*/ char const *attrname);

_CAMLIDL_EXTERN_C HDF_RESULT VSattrinfo(/*in*/ int vsid, /*in*/ int findex, /*in*/ intn attrindex, /*out*/ char *name, /*out*/ int *datatype, /*out*/ int *count, /*out*/ int *size);

_CAMLIDL_EXTERN_C intn VSisattr(/*in*/ int vsid);

_CAMLIDL_EXTERN_C int vicheckcompat(/*in*/ HFILEID f);

_CAMLIDL_EXTERN_C int vimakecompat(/*in*/ HFILEID f);

_CAMLIDL_EXTERN_C int vcheckcompat(/*in*/ char *fs);

_CAMLIDL_EXTERN_C int vmakecompat(/*in*/ char *fs);

_CAMLIDL_EXTERN_C int VSelts(/*in*/ int vkey);

_CAMLIDL_EXTERN_C int VSgetinterlace(/*in*/ int vkey);

_CAMLIDL_EXTERN_C HDF_RESULT VSsetinterlace(/*in*/ int vkey, /*in*/ int interlace);

_CAMLIDL_EXTERN_C int VSgetfields(/*in*/ int vkey, /*out*/ char *fields);

_CAMLIDL_EXTERN_C intn VSfexist(/*in*/ int vkey, /*in*/ char *fields);

_CAMLIDL_EXTERN_C int VSsizeof(/*in*/ int vkey, /*in*/ char *fields);

_CAMLIDL_EXTERN_C void VSdump(/*in*/ int vkey);

_CAMLIDL_EXTERN_C HDF_RESULT VSsetname(/*in*/ int vkey, /*in*/ char const *vsname);

_CAMLIDL_EXTERN_C HDF_RESULT VSsetclass(/*in*/ int vkey, /*in*/ char const *vsclass);

_CAMLIDL_EXTERN_C HDF_RESULT VSgetname(/*in*/ int vkey, /*out*/ char *vsname);

_CAMLIDL_EXTERN_C HDF_RESULT VSgetclass(/*in*/ int vkey, /*out*/ char *vsclass);

_CAMLIDL_EXTERN_C HDF_RESULT VSinquire(/*in*/ int vkey, /*out*/ int *nelt, /*out*/ int *interlace, /*out*/ char *fields, /*out*/ int *eltsize, /*out*/ char *vsname);

_CAMLIDL_EXTERN_C int VSlone(/*in*/ HFILEID f, /*in*/ int *idarray, /*in*/ int asize);

_CAMLIDL_EXTERN_C int Vlone(/*in*/ HFILEID f, /*in*/ int *idarray, /*in*/ int asize);

_CAMLIDL_EXTERN_C int Vfind(/*in*/ HFILEID f, /*in*/ char const *vgname);

_CAMLIDL_EXTERN_C int VSfind(/*in*/ HFILEID f, /*in*/ char const *vsname);

_CAMLIDL_EXTERN_C int Vfindclass(/*in*/ HFILEID f, /*in*/ char const *vgclass);

_CAMLIDL_EXTERN_C int VSfindclass(/*in*/ HFILEID f, /*in*/ char const *vsclass);

_CAMLIDL_EXTERN_C intn VSsetblocksize(/*in*/ int vkey, /*in*/ int block_size);

_CAMLIDL_EXTERN_C intn VSsetnumblocks(/*in*/ int vkey, /*in*/ int num_blocks);

_CAMLIDL_EXTERN_C intn VSgetblockinfo(/*in*/ int vkey, /*out*/ int *block_size, /*out*/ int *num_blocks);

_CAMLIDL_EXTERN_C void Vsetzap(void);

_CAMLIDL_EXTERN_C HDF_RESULT Vinitialize(/*in*/ HFILEID f);

_CAMLIDL_EXTERN_C HDF_RESULT Vfinish(/*in*/ HFILEID f);

_CAMLIDL_EXTERN_C HFILEID Vopen(/*in*/ char *path, /*in*/ intn acc_mode, /*in*/ hdf_int16 ndds);

_CAMLIDL_EXTERN_C intn Vclose(/*in*/ HFILEID f);

_CAMLIDL_EXTERN_C int Vattach(/*in*/ HFILEID f, /*in*/ int vgid, /*in*/ char const *accesstype);

_CAMLIDL_EXTERN_C int Vdetach(/*in*/ int vkey);

_CAMLIDL_EXTERN_C int Vinsert(/*in*/ int vkey, /*in*/ int vskey);

_CAMLIDL_EXTERN_C int Vsetname(/*in*/ int vkey, /*in*/ char const *vgname);

_CAMLIDL_EXTERN_C int Vsetclass(/*in*/ int vkey, /*in*/ char const *vgclass);

_CAMLIDL_EXTERN_C int Vgetid(/*in*/ HFILEID f, /*in*/ int vgid);

_CAMLIDL_EXTERN_C int Vgetnext(/*in*/ int vkey, /*in*/ int id);

_CAMLIDL_EXTERN_C int Vgetname(/*in*/ int vkey, /*out*/ char *vgname);

_CAMLIDL_EXTERN_C int Vgetclass(/*in*/ int vkey, /*out*/ char *vgclass);

_CAMLIDL_EXTERN_C intn Vinquire(/*in*/ int vkey, /*out*/ int *nentries, /*out*/ char *vgname);

_CAMLIDL_EXTERN_C int Vdelete(/*in*/ int f, /*in*/ int ref);

_CAMLIDL_EXTERN_C intn Vdeletetagref(/*in*/ int vkey, /*in*/ int tag, /*in*/ int ref);

_CAMLIDL_EXTERN_C int VSattach(/*in*/ HFILEID f, /*in*/ int vsref, /*in*/ char const *accesstype);

_CAMLIDL_EXTERN_C HDF_RESULT VSdetach(/*in*/ int vkey);

_CAMLIDL_EXTERN_C int VSQuerytag(/*in*/ int vkey);

_CAMLIDL_EXTERN_C int VSQueryref(/*in*/ int vkey);

_CAMLIDL_EXTERN_C int VSgetid(/*in*/ HFILEID f, /*in*/ int vsref);

_CAMLIDL_EXTERN_C int VSgetversion(/*in*/ int vkey);

_CAMLIDL_EXTERN_C int VSdelete(/*in*/ int f, /*in*/ int ref);

_CAMLIDL_EXTERN_C int VSappendable(/*in*/ int vkey, /*in*/ int blk);

_CAMLIDL_EXTERN_C HDF_RESULT VSsetfields(/*in*/ int vkey, /*in*/ char const *fields);

_CAMLIDL_EXTERN_C HDF_RESULT VSfdefine(/*in*/ int vkey, /*in*/ char const *field, /*in*/ hdf_data_type localtype, /*in*/ int order);

_CAMLIDL_EXTERN_C int VFnfields(/*in*/ int vkey);

_CAMLIDL_EXTERN_C char *VFfieldname(/*in*/ int vkey, /*in*/ int idx);

_CAMLIDL_EXTERN_C hdf_data_type VFfieldtype(/*in*/ int vkey, /*in*/ int idx);

_CAMLIDL_EXTERN_C int VFfieldisize(/*in*/ int vkey, /*in*/ int idx);

_CAMLIDL_EXTERN_C int VFfieldesize(/*in*/ int vkey, /*in*/ int idx);

_CAMLIDL_EXTERN_C int VFfieldorder(/*in*/ int vkey, /*in*/ int idx);

_CAMLIDL_EXTERN_C intn VSsetexternalfile(/*in*/ int vkey, /*in*/ char const *filename, /*in*/ int offset);

_CAMLIDL_EXTERN_C intn VSPshutdown(void);

_CAMLIDL_EXTERN_C int VSseek(/*in*/ int vkey, /*in*/ int eltpos);

typedef hdf_int16 gr_interlace_t;

typedef enum {
NOT_SDAPI_ID = -1,
SD_ID = 0,
SDS_ID,
DIM_ID,
} hdf_idtype_t;

_CAMLIDL_EXTERN_C int SDstart(/*in*/ char const *name, /*in*/ access_type accs);

_CAMLIDL_EXTERN_C HDF_RESULT SDend(/*in*/ int fid);

_CAMLIDL_EXTERN_C HDF_RESULT SDfileinfo(/*in*/ int fid, /*out*/ int *datasets, /*out*/ int *attrs);

_CAMLIDL_EXTERN_C int SDselect(/*in*/ int fid, /*in*/ int idx);

_CAMLIDL_EXTERN_C HDF_RESULT SDgetinfo(/*in*/ int sdsid, /*out*/ char *name, /*out*/ int *rank, /*out*/ int *dimsizes, /*out*/ int *nt, /*out*/ int *nattr);

_CAMLIDL_EXTERN_C int SDnametoindex(/*in*/ int fid, /*in*/ char const *name);

_CAMLIDL_EXTERN_C int SDcreate(/*in*/ int fid, /*in*/ char const *name, /*in*/ int nt, /*in*/ int rank, /*in*/ int *dimsizes);

_CAMLIDL_EXTERN_C int SDgetdimid(/*in*/ int sdsid, /*in*/ intn number);

_CAMLIDL_EXTERN_C intn SDsetdimname(/*in*/ int id, /*in*/ char const *name);

_CAMLIDL_EXTERN_C HDF_RESULT SDendaccess(/*in*/ int id);

_CAMLIDL_EXTERN_C HDF_RESULT SDattrinfo(/*in*/ int id, /*in*/ int idx, /*out*/ char *name, /*out*/ int *nt, /*out*/ int *count);

_CAMLIDL_EXTERN_C HDF_RESULT SDsetdatastrs(/*in*/ int sdsid, /*in*/ char const *l, /*in*/ char const *u, /*in*/ char const *f, /*in*/ char const *c);

_CAMLIDL_EXTERN_C intn SDsetcal(/*in*/ int sdsid, /*in*/ float64 cal, /*in*/ float64 cale, /*in*/ float64 ioff, /*in*/ float64 ioffe, /*in*/ int nt);

_CAMLIDL_EXTERN_C intn SDsetfillmode(/*in*/ int id, /*in*/ intn fillmode);

_CAMLIDL_EXTERN_C HDF_RESULT SDgetdatastrs(/*in*/ int sdsid, /*out*/ char *l, /*out*/ char *u, /*out*/ char *f, /*out*/ char *c, /*in*/ intn len);

_CAMLIDL_EXTERN_C intn SDgetcal(/*in*/ int sdsid, /*out*/ float64 *cal, /*out*/ float64 *cale, /*out*/ float64 *ioff, /*out*/ float64 *ioffe, /*out*/ int *nt);

_CAMLIDL_EXTERN_C intn SDsetdimstrs(/*in*/ int id, /*in*/ char const *l, /*in*/ char const *u, /*in*/ char const *f);

_CAMLIDL_EXTERN_C HDF_RESULT SDdiminfo(/*in*/ int id, /*out*/ char *name, /*out*/ int *size, /*out*/ int *nt, /*out*/ int *nattr);

_CAMLIDL_EXTERN_C intn SDgetdimstrs(/*in*/ int id, /*out*/ char *l, /*out*/ char *u, /*out*/ char *f, /*in*/ intn len);

_CAMLIDL_EXTERN_C intn SDsetexternalfile(/*in*/ int id, /*in*/ char const *filename, /*in*/ int offset);

_CAMLIDL_EXTERN_C intn SDsetnbitdataset(/*in*/ int id, /*in*/ intn start_bit, /*in*/ intn bit_len, /*in*/ intn sign_ext, /*in*/ intn fill_one);

_CAMLIDL_EXTERN_C HDF_RESULT SDsetcompress(/*in*/ int id, /*in*/ comp_coder_t type, /*in*/ comp_info *c_info);

_CAMLIDL_EXTERN_C HDF_RESULT SDgetcompress(/*in*/ int id, /*out*/ comp_coder_t *type, /*out*/ comp_info *c_info);

_CAMLIDL_EXTERN_C int SDfindattr(/*in*/ int id, /*in*/ char const *attrname);

_CAMLIDL_EXTERN_C int SDidtoref(/*in*/ int id);

_CAMLIDL_EXTERN_C int SDreftoindex(/*in*/ int fid, /*in*/ int ref);

_CAMLIDL_EXTERN_C int SDisrecord(/*in*/ int id);

_CAMLIDL_EXTERN_C intn SDiscoordvar(/*in*/ int id);

_CAMLIDL_EXTERN_C intn SDsetaccesstype(/*in*/ int id, /*in*/ uintn accesstype);

_CAMLIDL_EXTERN_C intn SDsetblocksize(/*in*/ int sdsid, /*in*/ int block_size);

_CAMLIDL_EXTERN_C intn SDsetdimval_comp(/*in*/ int dimid, /*in*/ intn compt_mode);

_CAMLIDL_EXTERN_C intn SDisdimval_bwcomp(/*in*/ int dimid);

_CAMLIDL_EXTERN_C int SDcheckempty(/*in*/ int sdsid, /*in*/ intn *emptySDS);

_CAMLIDL_EXTERN_C hdf_idtype_t SDidtype(/*in*/ int an_id);

_CAMLIDL_EXTERN_C intn SDsetchunkcache(/*in*/ int sdsid, /*in*/ int maxcache, /*in*/ int flags);

#ifdef _WIN32
#pragma pack(pop)
#endif


#endif /* !_CAMLIDL_HDF_WRAPPER_H */
