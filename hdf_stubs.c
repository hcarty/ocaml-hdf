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
// XXX END HORRIBLE HACK

/* For debugging - we want to have access to printf, stderr and such */
#include <stdio.h>
#include <string.h>

#include "hdf_stubs.h"

// Allow for ridiculously long exception strings... mainly in case of stupidly long filenames.
#define MAX_EXCEPTION_MESSAGE_LENGTH 10000

const char* hdf_datatype_to_string(int datatype) {
    switch (datatype) {
        case DFNT_FLOAT32:
            return "FLOAT32";
            break;
        case DFNT_FLOAT64:
            return "FLOAT64";
            break;
        case DFNT_INT8:
            return "INT8";
            break;
        case DFNT_UINT8:
            return "UINT8";
            break;
        case DFNT_INT16:
            return "INT16";
            break;
        case DFNT_UINT16:
            return "UINT16";
            break;
        case DFNT_INT32:
            return "INT32";
            break;
        case DFNT_UINT32:
            return "UINT32";
            break;
        case DFNT_CHAR8:
            return "CHAR8";
            break;
        default:
            {
                char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
                sprintf(exception_message, "Found an invalid data type: %d", datatype);
                caml_failwith(exception_message);
                // We won't get to this point, but whatever...
                return "FAIL";
            }
    }
}

int32 string_to_hdf_datatype(const char* datatype_string) {
    if ( strcmp(datatype_string, "FLOAT32") == 0 ) {
        return DFNT_FLOAT32;
    }
    else if ( strcmp(datatype_string, "FLOAT64") == 0 ) {
        return DFNT_FLOAT64;
    }
    else if ( strcmp(datatype_string, "INT8") == 0 ) {
        return DFNT_INT8;
    }
    else if ( strcmp(datatype_string, "UINT8") == 0 ) {
        return DFNT_UINT8;
    }
    else if ( strcmp(datatype_string, "INT16") == 0 ) {
        return DFNT_INT16;
    }
    else if ( strcmp(datatype_string, "UINT16") == 0 ) {
        return DFNT_UINT16;
    }
    else if ( strcmp(datatype_string, "INT32") == 0 ) {
        return DFNT_INT32;
    }
    else if ( strcmp(datatype_string, "UINT32") == 0 ) {
        return DFNT_UINT32;
    }
    else if ( strcmp(datatype_string, "CHAR8") == 0 ) {
        return DFNT_CHAR8;
    }
    else {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Found an invalid data type: %s", datatype_string);
        caml_failwith(exception_message);
        // We won't get to this point, but whatever...
        return -1;
    }
}

// For information on the following functions, see the HDF4 docs.

/* Test a filename to see if it is a proper HDF4 file. */
value ml_Hishdf(value filename) {
    CAMLparam1(filename);

    if (Hishdf( String_val(filename) ) == TRUE) {
        CAMLreturn( Val_true );
    }
    else {
        CAMLreturn( Val_false );
    }
}

/* Open a HDF file */
// XXX This opens the file read-only.  I'll fix that later.
// XXX Add exception checks
value ml_Hopen(value filename) {
    CAMLparam1(filename);

    CAMLreturn( caml_copy_int32( Hopen( String_val(filename), DFACC_READ, 0 ) ) );
}

/* Close a HDF file. */
value ml_Hclose(value file_id) {
    CAMLparam1(file_id);

    CAMLreturn( Val_int( Hclose( Int32_val(file_id) ) ) );
}

/* Open a path to the HDF SDS data interface. */
// XXX Add exception checks
value ml_SDstart(value filename) {
    CAMLparam1(filename);

    CAMLreturn( Val_int( SDstart( String_val(filename), DFACC_READ ) ) );
}

/* Close a connection started with SDstart. */
value ml_SDend(value sd_id) {
    CAMLparam1(sd_id);

    CAMLreturn( Val_int( SDend( Int_val(sd_id) ) ) );
}

/* Open a path to the HDF Vdata interface. */
// XXX Add exception checks
value ml_Vstart(value file_id) {
    CAMLparam1(file_id);

    CAMLreturn( caml_copy_int32( Vstart( Int32_val(file_id) ) ) );
}

/* Close a connection started with Vstart. */
// XXX Add exception checks
value ml_Vend(value file_id) {
    CAMLparam1(file_id);

    CAMLreturn( Val_int( Vend( Int32_val(file_id) ) ) );
}

/* Get the specs for a SDS data set. */
value ml_SDgetinfo(value sd_id, value sd_index) {
    CAMLparam2(sd_id, sd_index);
    CAMLlocal2 (information_tuple, dimension_array);

    // Get some basic information about this data.
    struct sd_info_struct sd_information;
    sd_information = c_SDgetinfo( Int_val(sd_id), Int_val(sd_index) );

    // Allocate an OCaml array to hold the dimensions of the HDF field.
    dimension_array = caml_alloc( sd_information.rank, 0 );
    int i;
    for ( i = 0; i < sd_information.rank; ++i ) {
        Store_field( dimension_array, i, Val_int(sd_information.dimsizes[i]) );
    }

    // Allocate space for the information from SDgetinfo, plus the sd_index.
    information_tuple = caml_alloc_tuple(5);
    Store_field( information_tuple, 0, sd_index);
    Store_field( information_tuple, 1, caml_copy_string(sd_information.sds_name) );
    Store_field( information_tuple, 2, caml_copy_string(sd_information.data_type_string) );
    Store_field( information_tuple, 3, dimension_array );
    Store_field( information_tuple, 4, Val_int(sd_information.num_attrs) );

    CAMLreturn (information_tuple);
}

/* A little wrapper around SDgetinfo. */
struct sd_info_struct c_SDgetinfo(int sd_id, int sd_index) {
    int32 sds_id;
    sds_id = SDselect(sd_id, sd_index);

    // Get some basic information about this data.
    struct sd_info_struct sd_information;
    int32 status;
    status = SDgetinfo(sds_id, sd_information.sds_name, &sd_information.rank, sd_information.dimsizes, &sd_information.data_type, &sd_information.num_attrs);

    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Unable to retrieve SD information: sd_id %d\n", Int_val(sd_id));
        caml_failwith(exception_message);
    }

    // Close everything up.
    status = SDendaccess(sds_id);

    strcpy( sd_information.data_type_string, hdf_datatype_to_string(sd_information.data_type) );

    return sd_information;
}

// XXX Add exception throwing for errors
value ml_SDreaddata(value sd_id, value sd_index, value data) {
    CAMLparam3(sd_id, sd_index, data);

    struct sd_info_struct sd_information;
    sd_information = c_SDgetinfo( Int_val(sd_id), Int_val(sd_index) );

    int32 sds_id;
    sds_id = SDselect( Int_val(sd_id), Int_val(sd_index) );

    int32 start[sd_information.rank];
    int32 edges[sd_information.rank];

    int num_elements;
    num_elements = 1;
    int i;
    for (i = 0; i < sd_information.rank; i++) {
        start[i] = 0;
        edges[i] = sd_information.dimsizes[i];
        num_elements *= sd_information.dimsizes[i];
    }

    // Read the data!
    int32 status;
    status = SDreaddata(sds_id, start, NULL, edges, Data_bigarray_val(data) ); 

    if (status == FAIL) {
        printf("Unable to read the SDS data with index %d.\n", sd_index);
        exit(-1);
    }

    // Close everything up.
    status = SDendaccess(sds_id);

    CAMLreturn( caml_copy_string( sd_information.data_type_string ) );
}

// Get the index of a given Vdata by name.
value ml_VSfind(value file_id, value vdata_name) {
    CAMLparam2(file_id, vdata_name);
    int32 index;
    index = VSfind(Int32_val(file_id), String_val(vdata_name));
    if (index == 0) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Can't get index from Vdata name: %s", String_val(vdata_name));
        caml_failwith(exception_message);
    }

    CAMLreturn( caml_copy_int32(index) );
}

// XXX Maybe this shouldn't throw an exception here?  I dunno...
int32 _vdata_ref_from_vd_index(int32 file_id, int32 vd_index) {
    // Select the proper vdata reference by counting from the first...
    // There may be a better way to do this, but this seems to be the easiest
    // at this time.
    int32 vdata_ref;
    vdata_ref = -1;
    int i;
    for (i = 0; i <= vd_index; i++) {
        vdata_ref = VSgetid(file_id, vdata_ref);
        if (vdata_ref == FAIL) {
            char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
            sprintf(exception_message, "Unable to attach to Vdata index: %d\n", vd_index);
            caml_failwith(exception_message);
        }
    }
    return vdata_ref;
}

// XXX Maybe this shouldn't throw an exception here?  I dunno...
int32 _vd_index_from_vdata_ref(int32 file_id, int32 target_vdata_ref) {
    // Select the proper vdata reference by counting from the first...
    // There may be a better way to do this, but this seems to be the easiest
    // at this time.
    int32 vdata_ref;
    vdata_ref = -1;
    int32 i;
    i = -1;
    while (vdata_ref != target_vdata_ref) {
        vdata_ref = VSgetid(file_id, vdata_ref);
        if (vdata_ref == FAIL) {
            char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
            sprintf(exception_message, "Unable to find Vdata ref: %d\n", target_vdata_ref);
            caml_failwith(exception_message);
        }
        i++;
    }
    return i;
}

value ml_vd_index_from_vdata_ref(value file_id, value target_vdata_ref) {
    CAMLparam2(file_id, target_vdata_ref);

    CAMLreturn( caml_copy_int32( _vd_index_from_vdata_ref(Int32_val(file_id), Int32_val(target_vdata_ref)) ) );
}

struct vs_info_struct c_VSinquire(int32 vd_index, int32 vdata_id) {
    // Get some information on the vdata.
    // XXX Using the "hdf_int32" type to make the compiler happy.  See the hack around the hdf.h include.
    struct vs_info_struct vs_info;
    int32 status;
    status = VSinquire(vdata_id, &vs_info.n_records, &vs_info.interlace_mode, vs_info.field_name_list, &vs_info.vdata_size, vs_info.vdata_name);
    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Can't get VSinquire for vd_index: %d", vd_index);
        caml_failwith(exception_message);
    }

    return vs_info;
}

/* Read VS data sets */
value ml_VSread(value file_id, value vd_index, value data) {
    CAMLparam3(file_id, vd_index, data);

    // Select the proper vdata reference by counting from the first...
    // There may be a better way to do this, but this seems to be the easiest
    // at this time.
    int32 vdata_ref;
    vdata_ref = _vdata_ref_from_vd_index( Int32_val(file_id), Int32_val(vd_index) );

    // Attach to the VData we want.
    int32 vdata_id;
    vdata_id = VSattach( Int32_val(file_id), vdata_ref, "r");
    if (vdata_id == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Something's wrong with the vdata_id...\n");
        caml_failwith(exception_message);
    }
/*
    else if ( VSisattr(vdata_id) == TRUE ) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf( exception_message, "This is an attribute, not data: %d\n", Int_val(vd_index) );
        caml_failwith(exception_message);
    }
*/

    // Get some information on the vdata.
    struct vs_info_struct vs_info;
    vs_info = c_VSinquire( Int32_val(vd_index), vdata_id );

    // Read the actual data from the file.
    int32 status;
    status = VSsetfields(vdata_id, vs_info.field_name_list);
    // XXX Add exception throwing in case of failure...

    // XXX I'm pretty sure FULL_INTERLACE is what we want here, but I'm not certain.
    status = VSread(vdata_id, Data_bigarray_val(data), vs_info.n_records, FULL_INTERLACE);
    if (status != vs_info.n_records) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Unable to read all of the reported Vdata records: %d of %d\n",
                status, vs_info.n_records);
        caml_failwith(exception_message);
    }

    // Close everything up.
    status = VSdetach(vdata_id);
    // XXX Add exception throwing in case of failure...

    CAMLreturn( caml_copy_string( vs_info.vdata_name ) );
}

value ml_VSinquire(value file_id, value vd_index) {
    CAMLparam2(file_id, vd_index);
    CAMLlocal1(information_tuple);

    // Select the proper vdata reference by counting from the first...
    // There may be a better way to do this, but this seems to be the easiest
    // at this time.
    int32 vdata_ref;
    vdata_ref = _vdata_ref_from_vd_index( Int32_val(file_id), Int32_val(vd_index) );

    // Attach to the VData we want.
    int32 vdata_id;
    vdata_id = VSattach( Int32_val(file_id), vdata_ref, "r");
    if (vdata_id == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Something's wrong with the vdata_id...\n");
        caml_failwith(exception_message);
    }

    // Get some information on the vdata.
    struct vs_info_struct vs_info;
    vs_info = c_VSinquire( Int32_val(vd_index), vdata_id );

    // Close everything up.
    int32 status;
    status = VSdetach(vdata_id);
    // XXX Add exception throwing in case of failure...

    // Allocate space for the information from SDgetinfo, plus the sd_index.
    information_tuple = caml_alloc_tuple(5);
    Store_field( information_tuple, 0, vd_index );
    Store_field( information_tuple, 1, caml_copy_string(vs_info.vdata_name) );
    Store_field( information_tuple, 2, caml_copy_string(vs_info.field_name_list) );
    Store_field( information_tuple, 3, Val_int(vs_info.vdata_size) );
    Store_field( information_tuple, 4, Val_int(vs_info.n_records) );

    CAMLreturn (information_tuple);
}

/**
 * HDF creation/writing routines.
 */

/* Create a new HDF file for writing */
// XXX This opens the file for writing only.  Existing files WILL BE DELETED.
// XXX Add exception checks
value ml_Hopen_create(value filename) {
    CAMLparam1(filename);

    CAMLreturn( Val_int( Hopen( String_val(filename), DFACC_CREATE, 0 ) ) );
}

/* Open an existing HDF file for read/write access
 * OR
 * if the file doesn't exist, create it.
 */
// XXX Add exception checks
value ml_Hopen_rw(value filename) {
    CAMLparam1(filename);

    CAMLreturn( caml_copy_int32( Hopen( String_val(filename), DFACC_WRITE, 0 ) ) );
}

/* Create a new SD file for writing.  Only really useful for pure-SD files. */
// XXX This opens the file for writing only.  Existing files WILL BE DELETED.
// XXX Add exception checks
value ml_SDstart_create(value filename) {
    CAMLparam1(filename);

    CAMLreturn( Val_int( SDstart( String_val(filename), DFACC_CREATE ) ) );
}

/* Open an EXISTING SD file for read/write access.  Returns an error if the file
 * does not exist.
 */
// XXX Add exception checks
value ml_SDstart_rw(value filename) {
    CAMLparam1(filename);

    CAMLreturn( Val_int( SDstart( String_val(filename), DFACC_WRITE ) ) );
}

value ml_SDcreate(value sd_id, value name, value data_type_string, value rank, value dimsizes) {
    CAMLparam5(sd_id, name, data_type_string, rank, dimsizes);

    int32 data_type;
    data_type = string_to_hdf_datatype( String_val(data_type_string) );

    CAMLreturn(
        Val_int(
            SDcreate(
                Int_val(sd_id),
                String_val(name),
                data_type,
                Int_val(rank),
                Data_bigarray_val(dimsizes)
            )
        )
    );
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
    status = SDwritedata( Int_val(sds_id), start, NULL, edge, Data_bigarray_val(data) );
    // XXX Check for errors, and throw those exceptions!!
    if (status == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Failure with SDwritedata.");
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

value ml_SDendaccess(value sds_id) {
    CAMLparam1(sds_id);

    CAMLreturn( Val_int( SDendaccess( Int_val( sds_id ) ) ) );
}

value ml_VSattach_create(value file_id) {
    CAMLparam1(file_id);

    int32 vdata_id;
    // Create a new vdata.
    vdata_id = VSattach( Int32_val(file_id), -1, "w" );

    if (vdata_id == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Unable to create new vdata.");
        caml_failwith(exception_message);
    }

    CAMLreturn( caml_copy_int32( vdata_id ) );
}

value ml_VSfdefine(value vdata_id, value fieldname, value data_type_string, value order) {
    CAMLparam4(vdata_id, fieldname, data_type_string, order);

    int32 data_type;
    data_type = string_to_hdf_datatype( String_val(data_type_string) );

    if ( VSfdefine( Int32_val(vdata_id), String_val(fieldname), data_type, Int_val(order) ) == FAIL ) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Failure defining new vdata field: %s", String_val(fieldname) );
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

value ml_VSsetfields(value vdata_id, value field_name_list) {
    CAMLparam2(vdata_id, field_name_list);

    int result;
    result = VSsetfields( Int32_val(vdata_id), String_val(field_name_list) );
    if (result == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Failure setting vdata fields: %s", String_val(field_name_list) );
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

// XXX Assumes a FULL_INTERLACE vdata buffer.
value ml_VSwrite(value vdata_id, value databuf, value n_records) {
    CAMLparam3(vdata_id, databuf, n_records);

    int records_written;
    records_written = VSwrite( Int32_val(vdata_id), (uchar8*)Data_bigarray_val(databuf), Int_val(n_records), FULL_INTERLACE);
    if (records_written == FAIL) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Failure with VSwrite.");
        caml_failwith(exception_message);
    }
    else if ( records_written != Int_val(n_records) ) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Written records (%d) not equal to given records (%d)", records_written, Int_val(n_records));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

value ml_VSsetname(value vdata_id, value vdata_name) {
    CAMLparam2(vdata_id, vdata_name);

    if ( VSsetname( Int32_val(vdata_id), String_val(vdata_name) ) == FAIL ) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Failure with VSsetname: %s", String_val(vdata_name));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

value ml_VSsetclass(value vdata_id, value vdata_class) {
    CAMLparam2(vdata_id, vdata_class);

    if ( VSsetclass( Int32_val(vdata_id), String_val(vdata_class) ) == FAIL ) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Failure with VSsetclass: %s", String_val(vdata_class));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}

value ml_VSdetach(value vdata_id) {
    CAMLparam1(vdata_id);

    if ( VSdetach( Int32_val(vdata_id) ) == FAIL ) {
        char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
        sprintf(exception_message, "Failure with VSdetach on id: %d", Int32_val(vdata_id));
        caml_failwith(exception_message);
    }

    CAMLreturn( Val_unit );
}
