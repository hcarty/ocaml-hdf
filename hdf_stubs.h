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

struct sd_info_struct c_SDgetinfo(int sd_id, int sd_index);
const char* hdf_getSDS_data(int sd_id, int sd_index, void* data);

