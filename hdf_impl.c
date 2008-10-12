/*
 * An OCaml wrapper for the HDF4 C library.
 * by Hezekiah M. Carty
 *
 * Based on example code in the OCaml 3.09.1 documentation.
 */

/* The "usual" OCaml includes */
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

/* XXX
 * This next bit of #define-ing and #undef-ing is a hack to get around some
 * typedef collisions between the OCaml and HDF4 headers.
 */
// XXX BEGIN HORRIBLE HACK
#undef int32
#undef uint32
#undef int8
#undef uint8
#undef int16
#undef uint16

#define int32 hdf_int32
#define uint32 hdf_uint32
#define int8 hdf_int8
#define uint8 hdf_uint8
#define int16 hdf_int16
#define uint16 hdf_uint16

#include <hdf.h>
#ifndef MAX_VAR_DIMS
#define MAX_VAR_DIMS H4_MAX_VAR_DIMS
#endif
// XXX END HORRIBLE HACK

/* For debugging - we want to have access to printf, stderr and such */
#include <stdio.h>
#include <string.h>

struct sd_info_struct {
    char sds_name[64];
    char data_type_string[32];
    int32 dimsizes[MAX_VAR_DIMS];
    int32 rank;
    int32 data_type;
    int32 num_attrs;
};

#define FIELD_SIZE 1000
struct vs_info_struct {
    char vdata_name[VSNAMELENMAX];
    char field_name_list[FIELD_SIZE];
    hdf_int32 vdata_size;
    hdf_int32 interlace_mode;
    hdf_int32 n_records;
};

// Allow for ridiculously long exception strings... mainly in case of stupidly long filenames.
#define MAX_EXCEPTION_MESSAGE_LENGTH 10000

//
//
// FUNCTION RETURN VALUE CHECKS (used by camlidl)
//
//

// Just raising an exception if an error is returned
void hdf_check_result(intn result) {
    if (result == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "HDF: General Error");
        caml_failwith(exception_message);
    }
    return;
}

void hdf_check_id(int32 result) {
    if (result == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "HDF: Bad ID");
        caml_failwith(exception_message);
    }
    return;
}

void hdf_check_info(int32 result) {
    if (result == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "HDF: Bad info");
        caml_failwith(exception_message);
    }
    return;
}

//
//
// OCAML VARIANTS TO/FROM HDF DATA TYPES
//
//

/* Called from OCaml (usually?).  This will give a variant which matches the
   given HDF data type value. */
value hdf_datatype_to_mlvariant(value datatype) {
    CAMLparam1(datatype);
    CAMLlocal1(datatype_variant);

#define CASE_HDF(hdf, txt)                      \
    if (Int32_val(datatype) == hdf) {           \
        datatype_variant = hash_variant(txt);   \
    }                                           \

    CASE_HDF(DFNT_FLOAT32, "float32")
    else CASE_HDF(DFNT_FLOAT64, "float64")
    else CASE_HDF(DFNT_INT8, "int8")
    else CASE_HDF(DFNT_UINT8, "uint8")
    else CASE_HDF(DFNT_INT16, "int16")
    else CASE_HDF(DFNT_UINT16, "uint16")
    else CASE_HDF(DFNT_INT32, "int32")
    else CASE_HDF(DFNT_UINT32, "uint32")
    else CASE_HDF(DFNT_CHAR8, "char8")
    else {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "hdf_datatype_to_mlvariant: Found an unsupported data type: %d", Int32_val(datatype));
        caml_failwith(exception_message);
        datatype_variant = Val_int(-1);
    }

#undef CASE_HDF

    CAMLreturn(datatype_variant);
}

value mlvariant_to_hdf_datatype(value datatype_variant) {
    CAMLparam1(datatype_variant);

    int32 datatype;
    int variant_number;
    variant_number = Int_val( hdf_datatype_to_mlvariant(datatype_variant) );

#define CASE_VARIANT(txt, hdf)                                  \
    if ( Int_val( hash_variant(txt) ) == variant_number ) {     \
        datatype = hdf;                                         \
    }                                                           \

    CASE_VARIANT("float32", DFNT_FLOAT32)
    else CASE_VARIANT("float64", DFNT_FLOAT64)
    else CASE_VARIANT("int8", DFNT_INT8)
    else CASE_VARIANT("uint8", DFNT_UINT8)
    else CASE_VARIANT("int16", DFNT_INT16)
    else CASE_VARIANT("uint16", DFNT_UINT16)
    else CASE_VARIANT("int32", DFNT_INT32)
    else CASE_VARIANT("uint32", DFNT_UINT32)
    else CASE_VARIANT("char8", DFNT_CHAR8)
    else {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "mlvariant_to_hdf_datatype: Found an unsupported data type: %d", variant_number);
        caml_failwith(exception_message);
        // This does not/should not matter... but it's here anyway.
        datatype = -1;
    }

#undef CASE_VARIANT

    CAMLreturn( caml_copy_int32(datatype) );
}

int caml_int32_array_length(value ml_array) {
    CAMLparam1(ml_array);

    CAMLreturnT(int, Wosize_val(ml_array));
}

void copy_caml_int32_array(value ml_array, int32* c_array) {
    CAMLparam1(ml_array);

    int len, i;
    len = caml_int32_array_length(ml_array);

    for (i = 0; i < len; i++) {
        c_array[i] = Int32_val( Field(ml_array, i) );
    }

    CAMLreturn0;
}

//
//
// SDS READING/WRITING
//
//

/* Read and write SDS entries */
value ml_SDreaddata(value sdsid, value ml_start, value ml_end, value data) {
    CAMLparam4(sdsid, ml_start, ml_end, data);

    int status;
    int32 start[caml_int32_array_length(ml_start)];
    int32 end[caml_int32_array_length(ml_end)];

    copy_caml_int32_array(ml_start, start);
    copy_caml_int32_array(ml_end, end);

    status = SDreaddata( Int32_val(sdsid), start, NULL, end, Data_bigarray_val(data) ); 
    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Error in SDreaddata for sdsid: %d\n", Int32_val(sdsid));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

value ml_SDwritedata(value sds_id, value data) {
    CAMLparam2(sds_id, data);

    int i;
    int num_dims = Bigarray_val(data)->num_dims;
    int32 start[num_dims];
    int32 edge[num_dims];
    for (i = 0; i < num_dims; i++) {
        start[i] = 0;
        edge[i] = Bigarray_val(data)->dim[i];
    }

    int32 status;
    status = SDwritedata( Int32_val(sds_id), start, NULL, edge, Data_bigarray_val(data) );
    // XXX Check for errors, and throw those exceptions!!
    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Error with SDwritedata.");
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

//
//
// VDATA READING/WRITING
//
//

/* Read and write VS data sets */
value ml_VSread(value vdata_id, value data, value n_records) {
    CAMLparam3(vdata_id, data, n_records);

    int32 status;
    status = VSread(Int32_val(vdata_id), Data_bigarray_val(data), Int32_val(n_records), FULL_INTERLACE);
    if (status != Int32_val(n_records)) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "VSread: Unable to read all of the requested Vdata records: %d of %d\n",
                status, Int32_val(n_records));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

// XXX Assumes a FULL_INTERLACE vdata buffer.
value ml_VSwrite(value vdata_id, value databuf, value n_records) {
    CAMLparam3(vdata_id, databuf, n_records);

    int records_written;
    records_written = VSwrite( Int32_val(vdata_id), (uchar8*)Data_bigarray_val(databuf), Int32_val(n_records), FULL_INTERLACE);
    if (records_written == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Error with VSwrite.");
        caml_failwith(exception_message);
    }
    else if ( records_written != Int32_val(n_records) ) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "VSwrote: Written records (%d) not equal to given records (%d)", records_written, Int32_val(n_records));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

//
//
// ATTRIBUTE READING/WRITING
//
//

value ml_SDreadattr(value id, value idx, value buf) {
    CAMLparam3(id, idx, buf);

    int status;
    status =
        SDreadattr( Int32_val(id), Int32_val(idx), Data_bigarray_val(buf) );
    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Error reading reading attribute, index: %d",
                Int32_val(idx));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

value ml_SDsetattr(value id, value name, value nt, value count, value data) {
    CAMLparam5(id, name, nt, count, data);

    int status;
    status =
        SDsetattr( Int32_val(id), String_val(name), Int32_val(nt),
                   Int32_val(count), Data_bigarray_val(data) );
    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Error reading setting attribute, name: %s",
                String_val(name));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

//
//
// FILL VALUE SETTING/GETTING
//
//

value _ml_SDgetfillvalue(value sdsid, int32 fill_type) {
    CAMLparam1(sdsid);

    // This will hold the fill value
    CAMLlocal1(fill);

    // Unless there is an error, one of these will be set to the proper fill value.
    double fill_float;
    int16 fill_int;
    int32 fill_int32;
    int status;
    switch (fill_type) {
        case DFNT_FLOAT64:
        case DFNT_FLOAT32:
            status = SDgetfillvalue(Int32_val(sdsid), (VOIDP)&fill_float);
            fill = caml_copy_double(fill_float);
            break;
        case DFNT_INT8:
        case DFNT_UINT8:
        case DFNT_INT16:
        case DFNT_UINT16:
            status = SDgetfillvalue(Int32_val(sdsid), (VOIDP)&fill_int);
            fill = Val_int(fill_int);
            break;
        case DFNT_INT32:
            status = SDgetfillvalue(Int32_val(sdsid), (VOIDP)&fill_int32);
            fill = caml_copy_int32(fill_int32);
            break;
        default: status = FAIL; break;
    }
    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Error getting SDS fill value.");
        caml_failwith(exception_message);
    }

    // WARNING!  This will have to be "cast" properly.
    CAMLreturn( fill );
}

// Now make OCaml-callable "type-safe" interfaces to the function...
value ml_SDgetfillvalue_float(value sdsid) {
    CAMLparam1(sdsid);

    // The specific type of float doesn't matter here
    CAMLreturn( _ml_SDgetfillvalue(sdsid, DFNT_FLOAT32) );
}

value ml_SDgetfillvalue_int(value sdsid) {
    CAMLparam1(sdsid);

    // The specific type of int doesn't matter here
    CAMLreturn( _ml_SDgetfillvalue(sdsid, DFNT_INT16) );
}

value ml_SDgetfillvalue_int32(value sdsid) {
    CAMLparam1(sdsid);

    // This, unlike the other functions, MUST be INT32
    CAMLreturn( _ml_SDgetfillvalue(sdsid, DFNT_INT32) );
}

value _ml_SDsetfillvalue(value sdsid, int32 fill_type, value fill) {
    CAMLparam2(sdsid, fill);

    // Unless there is an error, one of these will be set to the proper fill value.
    double fill_float;
    int fill_int;
    int32 fill_int32;
    int status;
    switch (fill_type) {
        default: status = FAIL; break;
        case DFNT_FLOAT64:
        case DFNT_FLOAT32:
            fill_float = Double_val(fill);
            status = SDsetfillvalue(Int32_val(sdsid), (VOIDP)&fill_float); break;
        case DFNT_INT8:
        case DFNT_UINT8:
        case DFNT_INT16:
        case DFNT_UINT16:
            fill_int = Int_val(fill);
            status = SDsetfillvalue(Int32_val(sdsid), (VOIDP)&fill_int); break;
        case DFNT_INT32:
            fill_int32 = Int32_val(fill);
            status = SDsetfillvalue(Int32_val(sdsid), (VOIDP)&fill_int32); break;
    }
    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Error setting SDS fill value.");
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

// Now make OCaml-callable "type-safe" interfaces to the function...
value ml_SDsetfillvalue_float(value sdsid, value fill) {
    CAMLparam2(sdsid, fill);

    // The specific type of float doesn't matter here
    CAMLreturn( _ml_SDsetfillvalue(sdsid, DFNT_FLOAT32, fill) );
}

value ml_SDsetfillvalue_int(value sdsid, value fill) {
    CAMLparam2(sdsid, fill);

    // The specific type of int doesn't matter here
    CAMLreturn( _ml_SDsetfillvalue(sdsid, DFNT_INT16, fill) );
}

value ml_SDsetfillvalue_int32(value sdsid, value fill) {
    CAMLparam2(sdsid, fill);

    // This, unlike the other functions, MUST be INT32
    CAMLreturn( _ml_SDsetfillvalue(sdsid, DFNT_INT32, fill) );
}

/*****************************
 TODO TODO TODO TODO TODO TODO
 *****************************
 The following functions have yet to be wrapped.
*/
//
//
// DIMENSION AND SCALE SETTING/GETTING
//
//

//    intn SDgetdimscale
//        (int32 id, void * data);

//    intn SDsetdimscale
//        (int32 id, int32 count, int32 nt, void * data);

//
//
// RANGE GETTING/SETTING
//
//

//    intn SDgetrange
//        (int32 sdsid, void * pmax, void * pmin);

//    intn SDsetrange
//        (int32 sdsid, void * pmax, void * pmin);
