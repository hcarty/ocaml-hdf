/* vd_ex3.c 
 * Create a single field order 3 Vdata.
 */

#include "hdf.h"

#define FIELD_NAME "Field Entries"
#define NUMBER_OF_ROWS 10
#define ORDER 3

main( ) 
{

     int32      file_id, vdata_id;
     intn       status, i;
     int16      vdata_buf[NUMBER_OF_ROWS * ORDER];

     /* Open the HDF file. */
     file_id = Hopen("VD_Ex3.hdf", DFACC_CREATE, 0);

     /* Initialize HDF Vgroup/Vdata interface. */
     status = Vstart(file_id);

     /* Create a new Vdata. */
     vdata_id = VSattach(file_id, -1, "w");

     /* Define the field data name, type and order. */
     status = VSfdefine(vdata_id, FIELD_NAME, DFNT_INT16, 
                            ORDER);

     /* Specify the field(s) that will be written to. */
     status = VSsetfields(vdata_id, FIELD_NAME);

     /* Generate the Vset data. */
     for (i = 0; i < NUMBER_OF_ROWS*ORDER; i+=ORDER) {
        vdata_buf[i] = i;
        vdata_buf[i + 1] = i + 1;
        vdata_buf[i + 2] = i + 2;
     }

     /* Write the data to the Vset. */
     status = VSwrite(vdata_id, (uchar8 *)vdata_buf, 
                      NUMBER_OF_ROWS, FULL_INTERLACE);

     /* Set the name and class. */
     status = VSsetname(vdata_id, "Example Vdata");
     status = VSsetclass(vdata_id, "Example Vdata");

     /* Terminate access to the Vdata. */
     status = VSdetach(vdata_id);

     /* Terminate access to the Vset interface and close the file. */
     status = Vend(file_id);
     status = Hclose(file_id);

}

