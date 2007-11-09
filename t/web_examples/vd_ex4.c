/* vd_ex4.c
 * Create a 4 field Vdata. Record values are stored in
 * an array of structure.
 * Use VDfpack to pack the field values fully interlaced
 * into a buffer.  Each VDfpack call packs 1 record.
 * Write out NRECORDS (20) records by 1 VSwrite call.
 */
 
#include "hdf.h"
#include "vg.h"
#include <stdio.h>

#define NRECORDS 20

#define FIELD_1 "Temp" 
#define FIELD_2 "Height" 
#define FIELD_3 "Speed" 
#define FIELD_4 "Ident"
#define FIELD_NAMES "Temp,Height,Speed,Ident"

main( ) 
{

     struct {
        float32      temp;
        int16        height;
        float32      speed;
        char         ident;
     } source[NRECORDS];

     int32      file_id, vdata_id, istat, msize = 0;
     uint8      *databuf, *pntr;
     int     	i, bufsize, recsize;
     VOIDP      fldbufpt[4];

     /* Open the HDF file. */
     file_id = Hopen("VD_Ex4.hdf", DFACC_CREATE, 0);

     /* Initialize the Vset interface. */
     istat = Vstart(file_id);

     /* Create a new Vdata. */
     vdata_id = VSattach(file_id, -1, "w");

     /* Define the field to write. */
     istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
     istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
     istat = VSfdefine(vdata_id, FIELD_3, DFNT_FLOAT32, 1); 
     istat = VSfdefine(vdata_id, FIELD_4, DFNT_CHAR8, 1); 

     /* Set the Vdata name. */
     istat=VSsetname(vdata_id, "Example Vset Name");

     /* Set the Vdata class. */
     istat=VSsetclass(vdata_id, "Example Vset Class");

     /* Set the field names. */
     istat = VSsetfields(vdata_id, FIELD_NAMES);

     recsize = (2 * sizeof(float32) + sizeof(int16) 
                + sizeof(char)); 
     bufsize = recsize * NRECORDS;
     databuf = (uint8 *) malloc(bufsize);
     if (databuf == NULL) {
         printf("malloc failed\n");
         exit;
     }
     pntr = databuf;
     /* set record values */
     for (i = 0; i < NRECORDS; i++) {
        source[i].temp = 1.11 * (i+1);
        source[i].height = i;
        source[i].speed = 1.11 * (i+1);
        source[i].ident = 'A' + i;
     }
     /* pack one record at a time */
     for (i = 0; i< NRECORDS; i++) {
        /* set field buf address */
        fldbufpt[0] = &source[i].temp;
        fldbufpt[1] = &source[i].height;
        fldbufpt[2] = &source[i].speed;
        fldbufpt[3] = &source[i].ident;
        /* pack field data into databuf */
        istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
                recsize, 1, NULL, fldbufpt);
        if (istat == FAIL)  {
            printf("VSfpack failed in packing record %d\n", i);
        }
        pntr = pntr + recsize; 
     }        

     /* Write the data to the Vset object. */
     istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 

     /* 
     * Terminate access to the Vdata, the Vset interface 
     * and the HDF file.
     */
     istat = VSdetach(vdata_id);
     istat = Vend(file_id);
     istat = Hclose(file_id);
     free(databuf);
}


