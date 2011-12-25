/* File generated from hdf_wrapper.idl */

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include "camlidlruntime.h"


#include "hdf_wrapper.h"

void camlidl_ml2c_hdf_wrapper__fcd(value _v1, _fcd * _c2, camlidl_ctx _ctx)
{
value _v3;
  if (_v1 == Val_int(0)) {
    (*_c2) = NULL;
  } else {
    _v3 = Field(_v1, 0);
    (*_c2) = (char  *) camlidl_malloc(sizeof(char ), _ctx);
    *(*_c2) = Int_val(_v3);
  }
}

value camlidl_c2ml_hdf_wrapper__fcd(_fcd * _c2, camlidl_ctx _ctx)
{
value _v1;
value _v3;
  if ((*_c2) == NULL) {
    _v1 = Val_int(0);
  } else {
    _v3 = Val_int((unsigned char)(*(*_c2)));
    Begin_root(_v3)
      _v1 = camlidl_alloc_small(1, 0);
      Field(_v1, 0) = _v3;
    End_roots();
  }
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_char8(value _v1, char8 * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_char8(char8 * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((unsigned char)((*_c2)));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_uchar8(value _v1, uchar8 * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_uchar8(uchar8 * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((*_c2));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_hdf_int8(value _v1, hdf_int8 * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_hdf_int8(hdf_int8 * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((unsigned char)((*_c2)));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_hdf_uint8(value _v1, hdf_uint8 * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_hdf_uint8(hdf_uint8 * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((*_c2));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_hdf_int16(value _v1, hdf_int16 * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_hdf_int16(hdf_int16 * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((*_c2));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_hdf_uint16(value _v1, hdf_uint16 * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_hdf_uint16(hdf_uint16 * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((*_c2));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_intn(value _v1, intn * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Long_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_intn(intn * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_long((*_c2));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_uintn(value _v1, uintn * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Long_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_uintn(uintn * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_long((*_c2));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_float32(value _v1, float32 * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Double_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_float32(float32 * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_double((*_c2));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_float64(value _v1, float64 * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Double_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_float64(float64 * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_double((*_c2));
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_HDF_RESULT(value _v1, HDF_RESULT * _c2, camlidl_ctx _ctx)
{
  camlidl_ml2c_hdf_wrapper_intn(_v1, &(*_c2), _ctx);
}

value camlidl_c2ml_hdf_wrapper_HDF_RESULT(HDF_RESULT * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_hdf_wrapper_intn(&(*_c2), _ctx);
  return _v1;
}

int camlidl_transl_table_hdf_wrapper_enum_1[59] = {
  DFNT_FLOAT32,
  DFNT_FLOAT,
  DFNT_FLOAT64,
  DFNT_DOUBLE,
  DFNT_FLOAT128,
  DFNT_INT8,
  DFNT_UINT8,
  DFNT_INT16,
  DFNT_UINT16,
  DFNT_INT32,
  DFNT_UINT32,
  DFNT_INT64,
  DFNT_UINT64,
  DFNT_INT128,
  DFNT_UINT128,
  DFNT_UCHAR8,
  DFNT_UCHAR,
  DFNT_CHAR8,
  DFNT_CHAR,
  DFNT_CHAR16,
  DFNT_UCHAR16,
  DFNT_NFLOAT32,
  DFNT_NFLOAT64,
  DFNT_NFLOAT128,
  DFNT_NINT8,
  DFNT_NUINT8,
  DFNT_NINT16,
  DFNT_NUINT16,
  DFNT_NINT32,
  DFNT_NUINT32,
  DFNT_NINT64,
  DFNT_NUINT64,
  DFNT_NINT128,
  DFNT_NUINT128,
  DFNT_NCHAR8,
  DFNT_NCHAR,
  DFNT_NUCHAR8,
  DFNT_NUCHAR,
  DFNT_NCHAR16,
  DFNT_NUCHAR16,
  DFNT_LFLOAT32,
  DFNT_LFLOAT64,
  DFNT_LFLOAT128,
  DFNT_LINT8,
  DFNT_LUINT8,
  DFNT_LINT16,
  DFNT_LUINT16,
  DFNT_LINT32,
  DFNT_LUINT32,
  DFNT_LINT64,
  DFNT_LUINT64,
  DFNT_LINT128,
  DFNT_LUINT128,
  DFNT_LCHAR8,
  DFNT_LCHAR,
  DFNT_LUCHAR8,
  DFNT_LUCHAR,
  DFNT_LCHAR16,
  DFNT_LUCHAR16,
};

void camlidl_ml2c_hdf_wrapper_hdf_data_type(value _v1, hdf_data_type * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_hdf_wrapper_enum_1[Int_val(_v1)];
}

value camlidl_c2ml_hdf_wrapper_hdf_data_type(hdf_data_type * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_find_enum((*_c2), camlidl_transl_table_hdf_wrapper_enum_1, 59, "typedef hdf_data_type: bad enum  value");
  return _v1;
}

int camlidl_transl_table_hdf_wrapper_enum_2[3] = {
  DFACC_READ,
  DFACC_WRITE,
  DFACC_CREATE,
};

void camlidl_ml2c_hdf_wrapper_access_type(value _v1, access_type * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_hdf_wrapper_enum_2[Int_val(_v1)];
}

value camlidl_c2ml_hdf_wrapper_access_type(access_type * _c2, camlidl_ctx _ctx)
{
value _v1;
  switch((*_c2)) {
  case DFACC_READ: _v1 = Val_int(0); break;
  case DFACC_WRITE: _v1 = Val_int(1); break;
  case DFACC_CREATE: _v1 = Val_int(2); break;
  default: invalid_argument("typedef access_type: bad enum  value");
  }
  return _v1;
}

int camlidl_transl_table_hdf_wrapper_enum_3[5] = {
  AN_UNDEF,
  AN_DATA_LABEL,
  AN_DATA_DESC,
  AN_FILE_LABEL,
  AN_FILE_DESC,
};

void camlidl_ml2c_hdf_wrapper_ann_type(value _v1, ann_type * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_hdf_wrapper_enum_3[Int_val(_v1)];
}

value camlidl_c2ml_hdf_wrapper_ann_type(ann_type * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_find_enum((*_c2), camlidl_transl_table_hdf_wrapper_enum_3, 5, "typedef ann_type: bad enum  value");
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_HFILEID(value _v1, HFILEID * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int32_val(_v1);
}

value camlidl_c2ml_hdf_wrapper_HFILEID(HFILEID * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_int32((*_c2));
  return _v1;
}

int camlidl_transl_table_hdf_wrapper_enum_4[1] = {
  COMP_MODEL_STDIO,
};

void camlidl_ml2c_hdf_wrapper_comp_model_t(value _v1, comp_model_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_hdf_wrapper_enum_4[Int_val(_v1)];
}

value camlidl_c2ml_hdf_wrapper_comp_model_t(comp_model_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  switch((*_c2)) {
  case COMP_MODEL_STDIO: _v1 = Val_int(0); break;
  default: invalid_argument("typedef comp_model_t: bad enum  value");
  }
  return _v1;
}

int camlidl_transl_table_hdf_wrapper_enum_5[8] = {
  COMP_CODE_NONE,
  COMP_CODE_RLE,
  COMP_CODE_NBIT,
  COMP_CODE_SKPHUFF,
  COMP_CODE_DEFLATE,
  COMP_CODE_SZIP,
  COMP_CODE_INVALID,
  COMP_CODE_JPEG,
};

void camlidl_ml2c_hdf_wrapper_comp_coder_t(value _v1, comp_coder_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_hdf_wrapper_enum_5[Int_val(_v1)];
}

value camlidl_c2ml_hdf_wrapper_comp_coder_t(comp_coder_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_find_enum((*_c2), camlidl_transl_table_hdf_wrapper_enum_5, 8, "typedef comp_coder_t: bad enum  value");
  return _v1;
}

int camlidl_transl_table_hdf_wrapper_enum_6[124] = {
  DFE_NONE,
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
};

void camlidl_ml2c_hdf_wrapper_hdf_err_code_t(value _v1, hdf_err_code_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_hdf_wrapper_enum_6[Int_val(_v1)];
}

value camlidl_c2ml_hdf_wrapper_hdf_err_code_t(hdf_err_code_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_find_enum((*_c2), camlidl_transl_table_hdf_wrapper_enum_6, 124, "typedef hdf_err_code_t: bad enum  value");
  return _v1;
}

void camlidl_ml2c_hdf_wrapper_comp_info(value _v1, comp_info * _c2, camlidl_ctx _ctx)
{
value _v3;
value _v4;
  _v3 = Field(_v1, 0);
  camlidl_ml2c_hdf_wrapper_intn(_v3, &(*_c2).skphuff.skp_size, _ctx);
  _v4 = Field(_v1, 1);
  camlidl_ml2c_hdf_wrapper_intn(_v4, &(*_c2).deflate.level, _ctx);
}

value camlidl_c2ml_hdf_wrapper_comp_info(comp_info * _c2, camlidl_ctx _ctx)
{
value _v1;
value _v3[2];
  _v3[0] = _v3[1] = 0;
  Begin_roots_block(_v3, 2)
    _v3[0] = camlidl_c2ml_hdf_wrapper_intn(&(*_c2).skphuff.skp_size, _ctx);
    _v3[1] = camlidl_c2ml_hdf_wrapper_intn(&(*_c2).deflate.level, _ctx);
    _v1 = camlidl_alloc_small(2, 0);
    Field(_v1, 0) = _v3[0];
    Field(_v1, 1) = _v3[1];
  End_roots()
  return _v1;
}

value camlidl_hdf_wrapper_Hopen(
	value _v_path,
	value _v_acc_mode,
	value _v_ndds)
{
  char const *path; /*in*/
  access_type acc_mode; /*in*/
  hdf_int16 ndds; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  path = String_val(_v_path);
  camlidl_ml2c_hdf_wrapper_access_type(_v_acc_mode, &acc_mode, _ctx);
  camlidl_ml2c_hdf_wrapper_hdf_int16(_v_ndds, &ndds, _ctx);
  _res = Hopen(path, acc_mode, ndds);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Hclose(
	value _v_file_id)
{
  int file_id; /*in*/
  HDF_RESULT _res;
  file_id = Int32_val(_v_file_id);
  _res = Hclose(file_id);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_Hishdf(
	value _v_filename)
{
  char const *filename; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  _res = Hishdf(filename);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_HEstring(
	value _v_error_code)
{
  hdf_err_code_t error_code; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_hdf_err_code_t(_v_error_code, &error_code, _ctx);
  _res = HEstring(error_code);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_HEvalue(
	value _v_level)
{
  int level; /*in*/
  hdf_err_code_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  level = Int32_val(_v_level);
  _res = HEvalue(level);
  _vres = camlidl_c2ml_hdf_wrapper_hdf_err_code_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_HEPclear(value _unit)
{
  HEPclear();
  return Val_unit;
}

value camlidl_hdf_wrapper_Vnattrs(
	value _v_vgid)
{
  int vgid; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vgid = Int32_val(_v_vgid);
  _res = Vnattrs(vgid);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vfindattr(
	value _v_vgid,
	value _v_attrname)
{
  int vgid; /*in*/
  char const *attrname; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vgid = Int32_val(_v_vgid);
  attrname = String_val(_v_attrname);
  _res = Vfindattr(vgid, attrname);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vattrinfo(
	value _v_vgid,
	value _v_attrindex)
{
  int vgid; /*in*/
  intn attrindex; /*in*/
  char *name; /*out*/
  int *datatype; /*out*/
  int *count; /*out*/
  int *size; /*out*/
  intn _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  int _c2;
  int _c3;
  value _vresult;
  value _vres[5] = { 0, 0, 0, 0, 0, };

  vgid = Int32_val(_v_vgid);
  camlidl_ml2c_hdf_wrapper_intn(_v_attrindex, &attrindex, _ctx);
  name = camlidl_malloc(1024 * sizeof(char ), _ctx);
  datatype = &_c1;
  count = &_c2;
  size = &_c3;
  _res = Vattrinfo(vgid, attrindex, name, datatype, count, size);
  Begin_roots_block(_vres, 5)
    _vres[0] = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
    _vres[1] = copy_string(name);
    _vres[2] = copy_int32(*datatype);
    _vres[3] = copy_int32(*count);
    _vres[4] = copy_int32(*size);
    _vresult = camlidl_alloc_small(5, 0);
    { mlsize_t _c4;
      for (_c4 = 0; _c4 < 5; _c4++) Field(_vresult, _c4) = _vres[_c4];
    }
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_Vgetversion(
	value _v_vgid)
{
  int vgid; /*in*/
  int _res;
  value _vres;

  vgid = Int32_val(_v_vgid);
  _res = Vgetversion(vgid);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSfindex(
	value _v_vsid,
	value _v_fieldname)
{
  int vsid; /*in*/
  char const *fieldname; /*in*/
  int *fldindex; /*out*/
  HDF_RESULT _res;
  int _c1;
  value _vres;

  vsid = Int32_val(_v_vsid);
  fieldname = String_val(_v_fieldname);
  fldindex = &_c1;
  _res = VSfindex(vsid, fieldname, fldindex);
  hdf_check_result(_res);
  _vres = copy_int32(*fldindex);
  return _vres;
}

value camlidl_hdf_wrapper_VSnattrs(
	value _v_vsid)
{
  int vsid; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vsid = Int32_val(_v_vsid);
  _res = VSnattrs(vsid);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSfnattrs(
	value _v_vsid,
	value _v_findex)
{
  int vsid; /*in*/
  int findex; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vsid = Int32_val(_v_vsid);
  findex = Int32_val(_v_findex);
  _res = VSfnattrs(vsid, findex);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSfindattr(
	value _v_vsid,
	value _v_findex,
	value _v_attrname)
{
  int vsid; /*in*/
  int findex; /*in*/
  char const *attrname; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vsid = Int32_val(_v_vsid);
  findex = Int32_val(_v_findex);
  attrname = String_val(_v_attrname);
  _res = VSfindattr(vsid, findex, attrname);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSattrinfo(
	value _v_vsid,
	value _v_findex,
	value _v_attrindex)
{
  int vsid; /*in*/
  int findex; /*in*/
  intn attrindex; /*in*/
  char *name; /*out*/
  int *datatype; /*out*/
  int *count; /*out*/
  int *size; /*out*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  int _c2;
  int _c3;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  vsid = Int32_val(_v_vsid);
  findex = Int32_val(_v_findex);
  camlidl_ml2c_hdf_wrapper_intn(_v_attrindex, &attrindex, _ctx);
  name = camlidl_malloc(1024 * sizeof(char ), _ctx);
  datatype = &_c1;
  count = &_c2;
  size = &_c3;
  _res = VSattrinfo(vsid, findex, attrindex, name, datatype, count, size);
  hdf_check_result(_res);
  Begin_roots_block(_vres, 4)
    _vres[0] = copy_string(name);
    _vres[1] = copy_int32(*datatype);
    _vres[2] = copy_int32(*count);
    _vres[3] = copy_int32(*size);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_VSisattr(
	value _v_vsid)
{
  int vsid; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vsid = Int32_val(_v_vsid);
  _res = VSisattr(vsid);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_vicheckcompat(
	value _v_f)
{
  HFILEID f; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  _res = vicheckcompat(f);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_vimakecompat(
	value _v_f)
{
  HFILEID f; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  _res = vimakecompat(f);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_vcheckcompat(
	value _v_fs)
{
  char *fs; /*in*/
  int _res;
  value _vres;

  fs = String_val(_v_fs);
  _res = vcheckcompat(fs);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_vmakecompat(
	value _v_fs)
{
  char *fs; /*in*/
  int _res;
  value _vres;

  fs = String_val(_v_fs);
  _res = vmakecompat(fs);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSelts(
	value _v_vkey)
{
  int vkey; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  _res = VSelts(vkey);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSgetinterlace(
	value _v_vkey)
{
  int vkey; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  _res = VSgetinterlace(vkey);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSsetinterlace(
	value _v_vkey,
	value _v_interlace)
{
  int vkey; /*in*/
  int interlace; /*in*/
  HDF_RESULT _res;
  vkey = Int32_val(_v_vkey);
  interlace = Int32_val(_v_interlace);
  _res = VSsetinterlace(vkey, interlace);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_VSgetfields(
	value _v_vkey)
{
  int vkey; /*in*/
  char *fields; /*out*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  vkey = Int32_val(_v_vkey);
  fields = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = VSgetfields(vkey, fields);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_int32(_res);
    _vres[1] = copy_string(fields);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_VSfexist(
	value _v_vkey,
	value _v_fields)
{
  int vkey; /*in*/
  char *fields; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  fields = String_val(_v_fields);
  _res = VSfexist(vkey, fields);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSsizeof(
	value _v_vkey,
	value _v_fields)
{
  int vkey; /*in*/
  char *fields; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  fields = String_val(_v_fields);
  _res = VSsizeof(vkey, fields);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSdump(
	value _v_vkey)
{
  int vkey; /*in*/
  vkey = Int32_val(_v_vkey);
  VSdump(vkey);
  return Val_unit;
}

value camlidl_hdf_wrapper_VSsetname(
	value _v_vkey,
	value _v_vsname)
{
  int vkey; /*in*/
  char const *vsname; /*in*/
  HDF_RESULT _res;
  vkey = Int32_val(_v_vkey);
  vsname = String_val(_v_vsname);
  _res = VSsetname(vkey, vsname);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_VSsetclass(
	value _v_vkey,
	value _v_vsclass)
{
  int vkey; /*in*/
  char const *vsclass; /*in*/
  HDF_RESULT _res;
  vkey = Int32_val(_v_vkey);
  vsclass = String_val(_v_vsclass);
  _res = VSsetclass(vkey, vsclass);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_VSgetname(
	value _v_vkey)
{
  int vkey; /*in*/
  char *vsname; /*out*/
  HDF_RESULT _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  vsname = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = VSgetname(vkey, vsname);
  hdf_check_result(_res);
  _vres = copy_string(vsname);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSgetclass(
	value _v_vkey)
{
  int vkey; /*in*/
  char *vsclass; /*out*/
  HDF_RESULT _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  vsclass = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = VSgetclass(vkey, vsclass);
  hdf_check_result(_res);
  _vres = copy_string(vsclass);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSinquire(
	value _v_vkey)
{
  int vkey; /*in*/
  int *nelt; /*out*/
  int *interlace; /*out*/
  char *fields; /*out*/
  int *eltsize; /*out*/
  char *vsname; /*out*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  int _c2;
  int _c3;
  value _vresult;
  value _vres[5] = { 0, 0, 0, 0, 0, };

  vkey = Int32_val(_v_vkey);
  nelt = &_c1;
  interlace = &_c2;
  fields = camlidl_malloc(1024 * sizeof(char ), _ctx);
  eltsize = &_c3;
  vsname = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = VSinquire(vkey, nelt, interlace, fields, eltsize, vsname);
  hdf_check_result(_res);
  Begin_roots_block(_vres, 5)
    _vres[0] = copy_int32(*nelt);
    _vres[1] = copy_int32(*interlace);
    _vres[2] = copy_string(fields);
    _vres[3] = copy_int32(*eltsize);
    _vres[4] = copy_string(vsname);
    _vresult = camlidl_alloc_small(5, 0);
    { mlsize_t _c4;
      for (_c4 = 0; _c4 < 5; _c4++) Field(_vresult, _c4) = _vres[_c4];
    }
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_VSlone(
	value _v_f,
	value _v_idarray,
	value _v_asize)
{
  HFILEID f; /*in*/
  int *idarray; /*in*/
  int asize; /*in*/
  int _res;
  value _v1;
  int _c2;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  if (_v_idarray == Val_int(0)) {
    idarray = NULL;
  } else {
    _v1 = Field(_v_idarray, 0);
    idarray = &_c2;
    _c2 = Int32_val(_v1);
  }
  asize = Int32_val(_v_asize);
  _res = VSlone(f, idarray, asize);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vlone(
	value _v_f,
	value _v_idarray,
	value _v_asize)
{
  HFILEID f; /*in*/
  int *idarray; /*in*/
  int asize; /*in*/
  int _res;
  value _v1;
  int _c2;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  if (_v_idarray == Val_int(0)) {
    idarray = NULL;
  } else {
    _v1 = Field(_v_idarray, 0);
    idarray = &_c2;
    _c2 = Int32_val(_v1);
  }
  asize = Int32_val(_v_asize);
  _res = Vlone(f, idarray, asize);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vfind(
	value _v_f,
	value _v_vgname)
{
  HFILEID f; /*in*/
  char const *vgname; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  vgname = String_val(_v_vgname);
  _res = Vfind(f, vgname);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSfind(
	value _v_f,
	value _v_vsname)
{
  HFILEID f; /*in*/
  char const *vsname; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  vsname = String_val(_v_vsname);
  _res = VSfind(f, vsname);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vfindclass(
	value _v_f,
	value _v_vgclass)
{
  HFILEID f; /*in*/
  char const *vgclass; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  vgclass = String_val(_v_vgclass);
  _res = Vfindclass(f, vgclass);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSfindclass(
	value _v_f,
	value _v_vsclass)
{
  HFILEID f; /*in*/
  char const *vsclass; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  vsclass = String_val(_v_vsclass);
  _res = VSfindclass(f, vsclass);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSsetblocksize(
	value _v_vkey,
	value _v_block_size)
{
  int vkey; /*in*/
  int block_size; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  block_size = Int32_val(_v_block_size);
  _res = VSsetblocksize(vkey, block_size);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSsetnumblocks(
	value _v_vkey,
	value _v_num_blocks)
{
  int vkey; /*in*/
  int num_blocks; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  num_blocks = Int32_val(_v_num_blocks);
  _res = VSsetnumblocks(vkey, num_blocks);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSgetblockinfo(
	value _v_vkey)
{
  int vkey; /*in*/
  int *block_size; /*out*/
  int *num_blocks; /*out*/
  intn _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  int _c2;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  vkey = Int32_val(_v_vkey);
  block_size = &_c1;
  num_blocks = &_c2;
  _res = VSgetblockinfo(vkey, block_size, num_blocks);
  Begin_roots_block(_vres, 3)
    _vres[0] = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
    _vres[1] = copy_int32(*block_size);
    _vres[2] = copy_int32(*num_blocks);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_Vsetzap(value _unit)
{
  Vsetzap();
  return Val_unit;
}

value camlidl_hdf_wrapper_Vinitialize(
	value _v_f)
{
  HFILEID f; /*in*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  _res = Vinitialize(f);
  hdf_check_result(_res);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_hdf_wrapper_Vfinish(
	value _v_f)
{
  HFILEID f; /*in*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  _res = Vfinish(f);
  hdf_check_result(_res);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_hdf_wrapper_Vopen(
	value _v_path,
	value _v_acc_mode,
	value _v_ndds)
{
  char *path; /*in*/
  intn acc_mode; /*in*/
  hdf_int16 ndds; /*in*/
  HFILEID _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  path = String_val(_v_path);
  camlidl_ml2c_hdf_wrapper_intn(_v_acc_mode, &acc_mode, _ctx);
  camlidl_ml2c_hdf_wrapper_hdf_int16(_v_ndds, &ndds, _ctx);
  _res = Vopen(path, acc_mode, ndds);
  _vres = camlidl_c2ml_hdf_wrapper_HFILEID(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vclose(
	value _v_f)
{
  HFILEID f; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  _res = Vclose(f);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vattach(
	value _v_f,
	value _v_vgid,
	value _v_accesstype)
{
  HFILEID f; /*in*/
  int vgid; /*in*/
  char const *accesstype; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  vgid = Int32_val(_v_vgid);
  accesstype = String_val(_v_accesstype);
  _res = Vattach(f, vgid, accesstype);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vdetach(
	value _v_vkey)
{
  int vkey; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  _res = Vdetach(vkey);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_Vinsert(
	value _v_vkey,
	value _v_vskey)
{
  int vkey; /*in*/
  int vskey; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  vskey = Int32_val(_v_vskey);
  _res = Vinsert(vkey, vskey);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_Vsetname(
	value _v_vkey,
	value _v_vgname)
{
  int vkey; /*in*/
  char const *vgname; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  vgname = String_val(_v_vgname);
  _res = Vsetname(vkey, vgname);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_Vsetclass(
	value _v_vkey,
	value _v_vgclass)
{
  int vkey; /*in*/
  char const *vgclass; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  vgclass = String_val(_v_vgclass);
  _res = Vsetclass(vkey, vgclass);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_Vgetid(
	value _v_f,
	value _v_vgid)
{
  HFILEID f; /*in*/
  int vgid; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  vgid = Int32_val(_v_vgid);
  _res = Vgetid(f, vgid);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_Vgetnext(
	value _v_vkey,
	value _v_id)
{
  int vkey; /*in*/
  int id; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  id = Int32_val(_v_id);
  _res = Vgetnext(vkey, id);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_Vgetname(
	value _v_vkey)
{
  int vkey; /*in*/
  char *vgname; /*out*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  vkey = Int32_val(_v_vkey);
  vgname = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = Vgetname(vkey, vgname);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_int32(_res);
    _vres[1] = copy_string(vgname);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_Vgetclass(
	value _v_vkey)
{
  int vkey; /*in*/
  char *vgclass; /*out*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  vkey = Int32_val(_v_vkey);
  vgclass = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = Vgetclass(vkey, vgclass);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_int32(_res);
    _vres[1] = copy_string(vgclass);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_Vinquire(
	value _v_vkey)
{
  int vkey; /*in*/
  int *nentries; /*out*/
  char *vgname; /*out*/
  intn _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  vkey = Int32_val(_v_vkey);
  nentries = &_c1;
  vgname = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = Vinquire(vkey, nentries, vgname);
  Begin_roots_block(_vres, 3)
    _vres[0] = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
    _vres[1] = copy_int32(*nentries);
    _vres[2] = copy_string(vgname);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_Vdelete(
	value _v_f,
	value _v_ref)
{
  int f; /*in*/
  int ref; /*in*/
  int _res;
  value _vres;

  f = Int32_val(_v_f);
  ref = Int32_val(_v_ref);
  _res = Vdelete(f, ref);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_Vdeletetagref(
	value _v_vkey,
	value _v_tag,
	value _v_ref)
{
  int vkey; /*in*/
  int tag; /*in*/
  int ref; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  tag = Int32_val(_v_tag);
  ref = Int32_val(_v_ref);
  _res = Vdeletetagref(vkey, tag, ref);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSattach(
	value _v_f,
	value _v_vsref,
	value _v_accesstype)
{
  HFILEID f; /*in*/
  int vsref; /*in*/
  char const *accesstype; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  vsref = Int32_val(_v_vsref);
  accesstype = String_val(_v_accesstype);
  _res = VSattach(f, vsref, accesstype);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSdetach(
	value _v_vkey)
{
  int vkey; /*in*/
  HDF_RESULT _res;
  vkey = Int32_val(_v_vkey);
  _res = VSdetach(vkey);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_VSQuerytag(
	value _v_vkey)
{
  int vkey; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  _res = VSQuerytag(vkey);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSQueryref(
	value _v_vkey)
{
  int vkey; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  _res = VSQueryref(vkey);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSgetid(
	value _v_f,
	value _v_vsref)
{
  HFILEID f; /*in*/
  int vsref; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_hdf_wrapper_HFILEID(_v_f, &f, _ctx);
  vsref = Int32_val(_v_vsref);
  _res = VSgetid(f, vsref);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSgetversion(
	value _v_vkey)
{
  int vkey; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  _res = VSgetversion(vkey);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSdelete(
	value _v_f,
	value _v_ref)
{
  int f; /*in*/
  int ref; /*in*/
  int _res;
  value _vres;

  f = Int32_val(_v_f);
  ref = Int32_val(_v_ref);
  _res = VSdelete(f, ref);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSappendable(
	value _v_vkey,
	value _v_blk)
{
  int vkey; /*in*/
  int blk; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  blk = Int32_val(_v_blk);
  _res = VSappendable(vkey, blk);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSsetfields(
	value _v_vkey,
	value _v_fields)
{
  int vkey; /*in*/
  char const *fields; /*in*/
  HDF_RESULT _res;
  vkey = Int32_val(_v_vkey);
  fields = String_val(_v_fields);
  _res = VSsetfields(vkey, fields);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_VSfdefine(
	value _v_vkey,
	value _v_field,
	value _v_localtype,
	value _v_order)
{
  int vkey; /*in*/
  char const *field; /*in*/
  hdf_data_type localtype; /*in*/
  int order; /*in*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  field = String_val(_v_field);
  camlidl_ml2c_hdf_wrapper_hdf_data_type(_v_localtype, &localtype, _ctx);
  order = Int32_val(_v_order);
  _res = VSfdefine(vkey, field, localtype, order);
  hdf_check_result(_res);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_hdf_wrapper_VFnfields(
	value _v_vkey)
{
  int vkey; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  _res = VFnfields(vkey);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VFfieldname(
	value _v_vkey,
	value _v_idx)
{
  int vkey; /*in*/
  int idx; /*in*/
  char *_res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  idx = Int32_val(_v_idx);
  _res = VFfieldname(vkey, idx);
  _vres = copy_string(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VFfieldtype(
	value _v_vkey,
	value _v_idx)
{
  int vkey; /*in*/
  int idx; /*in*/
  hdf_data_type _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  idx = Int32_val(_v_idx);
  _res = VFfieldtype(vkey, idx);
  _vres = camlidl_c2ml_hdf_wrapper_hdf_data_type(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VFfieldisize(
	value _v_vkey,
	value _v_idx)
{
  int vkey; /*in*/
  int idx; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  idx = Int32_val(_v_idx);
  _res = VFfieldisize(vkey, idx);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VFfieldesize(
	value _v_vkey,
	value _v_idx)
{
  int vkey; /*in*/
  int idx; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  idx = Int32_val(_v_idx);
  _res = VFfieldesize(vkey, idx);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VFfieldorder(
	value _v_vkey,
	value _v_idx)
{
  int vkey; /*in*/
  int idx; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  idx = Int32_val(_v_idx);
  _res = VFfieldorder(vkey, idx);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_VSsetexternalfile(
	value _v_vkey,
	value _v_filename,
	value _v_offset)
{
  int vkey; /*in*/
  char const *filename; /*in*/
  int offset; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  vkey = Int32_val(_v_vkey);
  filename = String_val(_v_filename);
  offset = Int32_val(_v_offset);
  _res = VSsetexternalfile(vkey, filename, offset);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSPshutdown(value _unit)
{
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _res = VSPshutdown();
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_VSseek(
	value _v_vkey,
	value _v_eltpos)
{
  int vkey; /*in*/
  int eltpos; /*in*/
  int _res;
  value _vres;

  vkey = Int32_val(_v_vkey);
  eltpos = Int32_val(_v_eltpos);
  _res = VSseek(vkey, eltpos);
  _vres = copy_int32(_res);
  return _vres;
}

void camlidl_ml2c_hdf_wrapper_gr_interlace_t(value _v1, gr_interlace_t * _c2, camlidl_ctx _ctx)
{
  camlidl_ml2c_hdf_wrapper_hdf_int16(_v1, &(*_c2), _ctx);
}

value camlidl_c2ml_hdf_wrapper_gr_interlace_t(gr_interlace_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_hdf_wrapper_hdf_int16(&(*_c2), _ctx);
  return _v1;
}

int camlidl_transl_table_hdf_wrapper_enum_10[4] = {
  NOT_SDAPI_ID,
  SD_ID,
  SDS_ID,
  DIM_ID,
};

void camlidl_ml2c_hdf_wrapper_hdf_idtype_t(value _v1, hdf_idtype_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_hdf_wrapper_enum_10[Int_val(_v1)];
}

value camlidl_c2ml_hdf_wrapper_hdf_idtype_t(hdf_idtype_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  switch((*_c2)) {
  case NOT_SDAPI_ID: _v1 = Val_int(0); break;
  case SD_ID: _v1 = Val_int(1); break;
  case SDS_ID: _v1 = Val_int(2); break;
  case DIM_ID: _v1 = Val_int(3); break;
  default: invalid_argument("typedef hdf_idtype_t: bad enum  value");
  }
  return _v1;
}

value camlidl_hdf_wrapper_SDstart(
	value _v_name,
	value _v_accs)
{
  char const *name; /*in*/
  access_type accs; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  name = String_val(_v_name);
  camlidl_ml2c_hdf_wrapper_access_type(_v_accs, &accs, _ctx);
  _res = SDstart(name, accs);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDend(
	value _v_fid)
{
  int fid; /*in*/
  HDF_RESULT _res;
  fid = Int32_val(_v_fid);
  _res = SDend(fid);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_SDfileinfo(
	value _v_fid)
{
  int fid; /*in*/
  int *datasets; /*out*/
  int *attrs; /*out*/
  HDF_RESULT _res;
  int _c1;
  int _c2;
  value _vresult;
  value _vres[2] = { 0, 0, };

  fid = Int32_val(_v_fid);
  datasets = &_c1;
  attrs = &_c2;
  _res = SDfileinfo(fid, datasets, attrs);
  hdf_check_result(_res);
  Begin_roots_block(_vres, 2)
    _vres[0] = copy_int32(*datasets);
    _vres[1] = copy_int32(*attrs);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  return _vresult;
}

value camlidl_hdf_wrapper_SDselect(
	value _v_fid,
	value _v_idx)
{
  int fid; /*in*/
  int idx; /*in*/
  int _res;
  value _vres;

  fid = Int32_val(_v_fid);
  idx = Int32_val(_v_idx);
  _res = SDselect(fid, idx);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_SDgetinfo(
	value _v_sdsid)
{
  int sdsid; /*in*/
  char *name; /*out*/
  int *rank; /*out*/
  int *dimsizes; /*out*/
  int *nt; /*out*/
  int *nattr; /*out*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  int _c2;
  int _c3;
  mlsize_t _c4;
  value _v5;
  value _vresult;
  value _vres[5] = { 0, 0, 0, 0, 0, };

  sdsid = Int32_val(_v_sdsid);
  name = camlidl_malloc(1024 * sizeof(char ), _ctx);
  rank = &_c1;
  dimsizes = camlidl_malloc(32 * sizeof(int ), _ctx);
  nt = &_c2;
  nattr = &_c3;
  _res = SDgetinfo(sdsid, name, rank, dimsizes, nt, nattr);
  hdf_check_result(_res);
  Begin_roots_block(_vres, 5)
    _vres[0] = copy_string(name);
    _vres[1] = copy_int32(*rank);
    _vres[2] = camlidl_alloc(32, 0);
    Begin_root(_vres[2])
      for (_c4 = 0; _c4 < 32; _c4++) {
        _v5 = copy_int32(dimsizes[_c4]);
        modify(&Field(_vres[2], _c4), _v5);
      }
    End_roots()
    _vres[3] = copy_int32(*nt);
    _vres[4] = copy_int32(*nattr);
    _vresult = camlidl_alloc_small(5, 0);
    { mlsize_t _c6;
      for (_c6 = 0; _c6 < 5; _c6++) Field(_vresult, _c6) = _vres[_c6];
    }
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_SDnametoindex(
	value _v_fid,
	value _v_name)
{
  int fid; /*in*/
  char const *name; /*in*/
  int _res;
  value _vres;

  fid = Int32_val(_v_fid);
  name = String_val(_v_name);
  _res = SDnametoindex(fid, name);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_SDcreate(
	value _v_fid,
	value _v_name,
	value _v_nt,
	value _v_dimsizes)
{
  int fid; /*in*/
  char const *name; /*in*/
  int nt; /*in*/
  int rank; /*in*/
  int *dimsizes; /*in*/
  int _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  fid = Int32_val(_v_fid);
  name = String_val(_v_name);
  nt = Int32_val(_v_nt);
  _c1 = Wosize_val(_v_dimsizes);
  dimsizes = camlidl_malloc(_c1 * sizeof(int ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_dimsizes, _c2);
    dimsizes[_c2] = Int32_val(_v3);
  }
  rank = _c1;
  _res = SDcreate(fid, name, nt, rank, dimsizes);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDgetdimid(
	value _v_sdsid,
	value _v_number)
{
  int sdsid; /*in*/
  intn number; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  sdsid = Int32_val(_v_sdsid);
  camlidl_ml2c_hdf_wrapper_intn(_v_number, &number, _ctx);
  _res = SDgetdimid(sdsid, number);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDsetdimname(
	value _v_id,
	value _v_name)
{
  int id; /*in*/
  char const *name; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  id = Int32_val(_v_id);
  name = String_val(_v_name);
  _res = SDsetdimname(id, name);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDendaccess(
	value _v_id)
{
  int id; /*in*/
  HDF_RESULT _res;
  id = Int32_val(_v_id);
  _res = SDendaccess(id);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_SDattrinfo(
	value _v_id,
	value _v_idx)
{
  int id; /*in*/
  int idx; /*in*/
  char *name; /*out*/
  int *nt; /*out*/
  int *count; /*out*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  int _c2;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  id = Int32_val(_v_id);
  idx = Int32_val(_v_idx);
  name = camlidl_malloc(1024 * sizeof(char ), _ctx);
  nt = &_c1;
  count = &_c2;
  _res = SDattrinfo(id, idx, name, nt, count);
  hdf_check_result(_res);
  Begin_roots_block(_vres, 3)
    _vres[0] = copy_string(name);
    _vres[1] = copy_int32(*nt);
    _vres[2] = copy_int32(*count);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_SDsetdatastrs(
	value _v_sdsid,
	value _v_l,
	value _v_u,
	value _v_f,
	value _v_c)
{
  int sdsid; /*in*/
  char const *l; /*in*/
  char const *u; /*in*/
  char const *f; /*in*/
  char const *c; /*in*/
  HDF_RESULT _res;
  sdsid = Int32_val(_v_sdsid);
  l = String_val(_v_l);
  u = String_val(_v_u);
  f = String_val(_v_f);
  c = String_val(_v_c);
  _res = SDsetdatastrs(sdsid, l, u, f, c);
  hdf_check_result(_res);
  return Val_unit;
}

value camlidl_hdf_wrapper_SDsetcal(
	value _v_sdsid,
	value _v_cal,
	value _v_cale,
	value _v_ioff,
	value _v_ioffe,
	value _v_nt)
{
  int sdsid; /*in*/
  float64 cal; /*in*/
  float64 cale; /*in*/
  float64 ioff; /*in*/
  float64 ioffe; /*in*/
  int nt; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  sdsid = Int32_val(_v_sdsid);
  camlidl_ml2c_hdf_wrapper_float64(_v_cal, &cal, _ctx);
  camlidl_ml2c_hdf_wrapper_float64(_v_cale, &cale, _ctx);
  camlidl_ml2c_hdf_wrapper_float64(_v_ioff, &ioff, _ctx);
  camlidl_ml2c_hdf_wrapper_float64(_v_ioffe, &ioffe, _ctx);
  nt = Int32_val(_v_nt);
  _res = SDsetcal(sdsid, cal, cale, ioff, ioffe, nt);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDsetcal_bytecode(value * argv, int argn)
{
  return camlidl_hdf_wrapper_SDsetcal(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_hdf_wrapper_SDsetfillmode(
	value _v_id,
	value _v_fillmode)
{
  int id; /*in*/
  intn fillmode; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  id = Int32_val(_v_id);
  camlidl_ml2c_hdf_wrapper_intn(_v_fillmode, &fillmode, _ctx);
  _res = SDsetfillmode(id, fillmode);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDgetdatastrs(
	value _v_sdsid,
	value _v_len)
{
  int sdsid; /*in*/
  char *l; /*out*/
  char *u; /*out*/
  char *f; /*out*/
  char *c; /*out*/
  intn len; /*in*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  sdsid = Int32_val(_v_sdsid);
  camlidl_ml2c_hdf_wrapper_intn(_v_len, &len, _ctx);
  l = camlidl_malloc(1024 * sizeof(char ), _ctx);
  u = camlidl_malloc(1024 * sizeof(char ), _ctx);
  f = camlidl_malloc(1024 * sizeof(char ), _ctx);
  c = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = SDgetdatastrs(sdsid, l, u, f, c, len);
  hdf_check_result(_res);
  Begin_roots_block(_vres, 4)
    _vres[0] = copy_string(l);
    _vres[1] = copy_string(u);
    _vres[2] = copy_string(f);
    _vres[3] = copy_string(c);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_SDgetcal(
	value _v_sdsid)
{
  int sdsid; /*in*/
  float64 *cal; /*out*/
  float64 *cale; /*out*/
  float64 *ioff; /*out*/
  float64 *ioffe; /*out*/
  int *nt; /*out*/
  intn _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  float64 _c1;
  float64 _c2;
  float64 _c3;
  float64 _c4;
  int _c5;
  value _vresult;
  value _vres[6] = { 0, 0, 0, 0, 0, 0, };

  sdsid = Int32_val(_v_sdsid);
  cal = &_c1;
  cale = &_c2;
  ioff = &_c3;
  ioffe = &_c4;
  nt = &_c5;
  _res = SDgetcal(sdsid, cal, cale, ioff, ioffe, nt);
  Begin_roots_block(_vres, 6)
    _vres[0] = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
    _vres[1] = camlidl_c2ml_hdf_wrapper_float64(&*cal, _ctx);
    _vres[2] = camlidl_c2ml_hdf_wrapper_float64(&*cale, _ctx);
    _vres[3] = camlidl_c2ml_hdf_wrapper_float64(&*ioff, _ctx);
    _vres[4] = camlidl_c2ml_hdf_wrapper_float64(&*ioffe, _ctx);
    _vres[5] = copy_int32(*nt);
    _vresult = camlidl_alloc_small(6, 0);
    { mlsize_t _c6;
      for (_c6 = 0; _c6 < 6; _c6++) Field(_vresult, _c6) = _vres[_c6];
    }
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_SDsetdimstrs(
	value _v_id,
	value _v_l,
	value _v_u,
	value _v_f)
{
  int id; /*in*/
  char const *l; /*in*/
  char const *u; /*in*/
  char const *f; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  id = Int32_val(_v_id);
  l = String_val(_v_l);
  u = String_val(_v_u);
  f = String_val(_v_f);
  _res = SDsetdimstrs(id, l, u, f);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDdiminfo(
	value _v_id)
{
  int id; /*in*/
  char *name; /*out*/
  int *size; /*out*/
  int *nt; /*out*/
  int *nattr; /*out*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  int _c2;
  int _c3;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  id = Int32_val(_v_id);
  name = camlidl_malloc(1024 * sizeof(char ), _ctx);
  size = &_c1;
  nt = &_c2;
  nattr = &_c3;
  _res = SDdiminfo(id, name, size, nt, nattr);
  hdf_check_result(_res);
  Begin_roots_block(_vres, 4)
    _vres[0] = copy_string(name);
    _vres[1] = copy_int32(*size);
    _vres[2] = copy_int32(*nt);
    _vres[3] = copy_int32(*nattr);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_SDgetdimstrs(
	value _v_id,
	value _v_len)
{
  int id; /*in*/
  char *l; /*out*/
  char *u; /*out*/
  char *f; /*out*/
  intn len; /*in*/
  intn _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  id = Int32_val(_v_id);
  camlidl_ml2c_hdf_wrapper_intn(_v_len, &len, _ctx);
  l = camlidl_malloc(1024 * sizeof(char ), _ctx);
  u = camlidl_malloc(1024 * sizeof(char ), _ctx);
  f = camlidl_malloc(1024 * sizeof(char ), _ctx);
  _res = SDgetdimstrs(id, l, u, f, len);
  Begin_roots_block(_vres, 4)
    _vres[0] = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
    _vres[1] = copy_string(l);
    _vres[2] = copy_string(u);
    _vres[3] = copy_string(f);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_SDsetexternalfile(
	value _v_id,
	value _v_filename,
	value _v_offset)
{
  int id; /*in*/
  char const *filename; /*in*/
  int offset; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  id = Int32_val(_v_id);
  filename = String_val(_v_filename);
  offset = Int32_val(_v_offset);
  _res = SDsetexternalfile(id, filename, offset);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDsetnbitdataset(
	value _v_id,
	value _v_start_bit,
	value _v_bit_len,
	value _v_sign_ext,
	value _v_fill_one)
{
  int id; /*in*/
  intn start_bit; /*in*/
  intn bit_len; /*in*/
  intn sign_ext; /*in*/
  intn fill_one; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  id = Int32_val(_v_id);
  camlidl_ml2c_hdf_wrapper_intn(_v_start_bit, &start_bit, _ctx);
  camlidl_ml2c_hdf_wrapper_intn(_v_bit_len, &bit_len, _ctx);
  camlidl_ml2c_hdf_wrapper_intn(_v_sign_ext, &sign_ext, _ctx);
  camlidl_ml2c_hdf_wrapper_intn(_v_fill_one, &fill_one, _ctx);
  _res = SDsetnbitdataset(id, start_bit, bit_len, sign_ext, fill_one);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDsetcompress(
	value _v_id,
	value _v_type,
	value _v_c_info)
{
  int id; /*in*/
  comp_coder_t type; /*in*/
  comp_info *c_info; /*in*/
  HDF_RESULT _res;
  comp_info _c1;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  id = Int32_val(_v_id);
  camlidl_ml2c_hdf_wrapper_comp_coder_t(_v_type, &type, _ctx);
  c_info = &_c1;
  camlidl_ml2c_hdf_wrapper_comp_info(_v_c_info, &_c1, _ctx);
  _res = SDsetcompress(id, type, c_info);
  hdf_check_result(_res);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_hdf_wrapper_SDgetcompress(
	value _v_id)
{
  int id; /*in*/
  comp_coder_t *type; /*out*/
  comp_info *c_info; /*out*/
  HDF_RESULT _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  comp_coder_t _c1;
  comp_info _c2;
  value _vresult;
  value _vres[2] = { 0, 0, };

  id = Int32_val(_v_id);
  type = &_c1;
  c_info = &_c2;
  _res = SDgetcompress(id, type, c_info);
  hdf_check_result(_res);
  Begin_roots_block(_vres, 2)
    _vres[0] = camlidl_c2ml_hdf_wrapper_comp_coder_t(&*type, _ctx);
    _vres[1] = camlidl_c2ml_hdf_wrapper_comp_info(&*c_info, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_hdf_wrapper_SDfindattr(
	value _v_id,
	value _v_attrname)
{
  int id; /*in*/
  char const *attrname; /*in*/
  int _res;
  value _vres;

  id = Int32_val(_v_id);
  attrname = String_val(_v_attrname);
  _res = SDfindattr(id, attrname);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_SDidtoref(
	value _v_id)
{
  int id; /*in*/
  int _res;
  value _vres;

  id = Int32_val(_v_id);
  _res = SDidtoref(id);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_SDreftoindex(
	value _v_fid,
	value _v_ref)
{
  int fid; /*in*/
  int ref; /*in*/
  int _res;
  value _vres;

  fid = Int32_val(_v_fid);
  ref = Int32_val(_v_ref);
  _res = SDreftoindex(fid, ref);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_SDisrecord(
	value _v_id)
{
  int id; /*in*/
  int _res;
  value _vres;

  id = Int32_val(_v_id);
  _res = SDisrecord(id);
  _vres = copy_int32(_res);
  return _vres;
}

value camlidl_hdf_wrapper_SDiscoordvar(
	value _v_id)
{
  int id; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  id = Int32_val(_v_id);
  _res = SDiscoordvar(id);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDsetaccesstype(
	value _v_id,
	value _v_accesstype)
{
  int id; /*in*/
  uintn accesstype; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  id = Int32_val(_v_id);
  camlidl_ml2c_hdf_wrapper_uintn(_v_accesstype, &accesstype, _ctx);
  _res = SDsetaccesstype(id, accesstype);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDsetblocksize(
	value _v_sdsid,
	value _v_block_size)
{
  int sdsid; /*in*/
  int block_size; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  sdsid = Int32_val(_v_sdsid);
  block_size = Int32_val(_v_block_size);
  _res = SDsetblocksize(sdsid, block_size);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDsetdimval_comp(
	value _v_dimid,
	value _v_compt_mode)
{
  int dimid; /*in*/
  intn compt_mode; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  dimid = Int32_val(_v_dimid);
  camlidl_ml2c_hdf_wrapper_intn(_v_compt_mode, &compt_mode, _ctx);
  _res = SDsetdimval_comp(dimid, compt_mode);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDisdimval_bwcomp(
	value _v_dimid)
{
  int dimid; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  dimid = Int32_val(_v_dimid);
  _res = SDisdimval_bwcomp(dimid);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDcheckempty(
	value _v_sdsid,
	value _v_emptySDS)
{
  int sdsid; /*in*/
  intn *emptySDS; /*in*/
  int _res;
  value _v1;
  intn _c2;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  sdsid = Int32_val(_v_sdsid);
  if (_v_emptySDS == Val_int(0)) {
    emptySDS = NULL;
  } else {
    _v1 = Field(_v_emptySDS, 0);
    emptySDS = &_c2;
    camlidl_ml2c_hdf_wrapper_intn(_v1, &_c2, _ctx);
  }
  _res = SDcheckempty(sdsid, emptySDS);
  _vres = copy_int32(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDidtype(
	value _v_an_id)
{
  int an_id; /*in*/
  hdf_idtype_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  an_id = Int32_val(_v_an_id);
  _res = SDidtype(an_id);
  _vres = camlidl_c2ml_hdf_wrapper_hdf_idtype_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_hdf_wrapper_SDsetchunkcache(
	value _v_sdsid,
	value _v_maxcache,
	value _v_flags)
{
  int sdsid; /*in*/
  int maxcache; /*in*/
  int flags; /*in*/
  intn _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  sdsid = Int32_val(_v_sdsid);
  maxcache = Int32_val(_v_maxcache);
  flags = Int32_val(_v_flags);
  _res = SDsetchunkcache(sdsid, maxcache, flags);
  _vres = camlidl_c2ml_hdf_wrapper_intn(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

