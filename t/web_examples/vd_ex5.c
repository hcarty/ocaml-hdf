/* vd_ex5.c
 * Read in the Vdata from the file created by vd_ex4.c.
 * Call VSfpack to unpack the record into  field value
 * buffers. Print out the contents of the buffers.
 */

#include "hdf.h"
#include "vg.h"

#define NRECORDS 20

void main(void)
{

     char    vdata_name[MAX_NC_NAME], vdata_class[MAX_NC_NAME];
     char    fields[60];
     int32   file_id, vdata_id, istat;
     int32   n_records, interlace, vdata_size, vdata_ref;
     int     bufsz = (2 * sizeof(float32) + sizeof(char) \
                         + sizeof(int16)) * NRECORDS;
     int    i;
     uint8  databuf[((2 * sizeof(float32)) + sizeof(char) \
                         + sizeof(int16)) * NRECORDS];
     VOIDP fldbufptrs[4];
     float32 itemp[NRECORDS], ispeed[NRECORDS];
     int16   iheight[NRECORDS];
     char    idents[NRECORDS];

     /* Open the HDF file. */
     file_id = Hopen("VD_Ex4.hdf", DFACC_READ, 0);

     /* Initialize the Vset interface. */
     istat = Vstart(file_id);

     /* 
     * Get the reference number for the first Vdata in 
     * the file. 
     */
     vdata_ref = -1;
     vdata_ref = VSgetid(file_id, vdata_ref);

     /* Attach to the first Vdata in read mode. */
     vdata_id = VSattach(file_id, vdata_ref, "r");
     for (i=0; i<60; i++)
         fields[i] = '\0';
     /* Get the list of field names. */
     istat =VSinquire(vdata_id, &n_records, &interlace, 
            fields, &vdata_size, vdata_name);
     printf("files: %s, n_records: %d, vdata_size: %d\n",
              fields, n_records, vdata_size);
     /* Get the class. */
     istat = VSgetclass(vdata_id, vdata_class);

     /* Determine the fields that will be read. */
     istat = VSsetfields(vdata_id, fields);
     
     /* Print the Vdata information. */
     printf("Current Vdata name: %s \nCurrent Vdata class: %s.\n", 
             vdata_name, vdata_class);

     /* Read the data. */
     istat = VSread(vdata_id, (VOIDP)databuf, n_records,
                     FULL_INTERLACE);
     /* set fldbufptrs and unpack field values */
     fldbufptrs[0] = &idents[0];
     fldbufptrs[1] = &ispeed[0];
     fldbufptrs[2] = &iheight[0];
     fldbufptrs[3] = &itemp[0];
     
     istat = VSfpack(vdata_id, _HDF_VSUNPACK, fields, databuf,
                 bufsz, n_records, "Ident,Speed,Height,Temp",
                      fldbufptrs);
         printf("     Temp      Height     Speed      Ident\n");
     for (i=0; i < n_records; i++) {
         printf("   %6.2f    %6d      %6.2f       %c\n",
                   itemp[i],iheight[i],ispeed[i],idents[i]);
     }
     /* Detach from the Vdata, close the interface and the file. */
     istat = VSdetach(vdata_id);
     istat = Vend(file_id);
     istat = Hclose(file_id);

}
