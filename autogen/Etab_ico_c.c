/* http://stupefydeveloper.blogspot.ru/2008/08/cc-embed-binary-data-into-elf.html */
#include <urweb.h>
#include <stdio.h>
#include "Etab_ico_c.h"

#define BLOBSZ 142
static char blob[BLOBSZ];

uw_Basis_blob uw_Etab_ico_c_blob (uw_context ctx, uw_unit unit)
{
  uw_Basis_blob uwblob;
  uwblob.data = &blob[0];
  uwblob.size = BLOBSZ;
  return uwblob;
}

uw_Basis_string uw_Etab_ico_c_text (uw_context ctx, uw_unit unit) {
  char* data = &blob[0];
  size_t size = sizeof(blob);
  char * c = uw_malloc(ctx, size+1);
  char * write = c;
  int i;
  for (i = 0; i < size; i++) {
    *write =  data[i];
    if (*write == '\0')
      *write = '\n';
    *write++;
  }
  *write=0;
  return c;
  }

static char blob[BLOBSZ] = {
0x00 ,0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x04 ,0x04 ,0x00 ,0x00 ,0x01 ,0x00 ,0x20 ,0x00 ,0x78 ,0x00 ,0x00 ,0x00 ,0x16 ,0x00 ,0x00 ,0x00 ,0x28 ,0x00 ,0x00 ,0x00 ,0x04 ,0x00 ,0x00 ,0x00 ,0x08 ,0x00 ,0x00 ,0x00 ,0x01 ,0x00 ,0x20 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0xC1 ,0xD8 ,0xD0 ,0xFF ,0xC1 ,0xD8 ,0xD0 ,0xFF ,0x73 ,0xB2 ,0xBF ,0xFF ,0x73 ,0xB2 ,0xBF ,0xFF ,0xC1 ,0xD8 ,0xD0 ,0xFF ,0xC1 ,0xD8 ,0xD0 ,0xFF ,0x73 ,0xB2 ,0xBF ,0xFF ,0x73 ,0xB2 ,0xBF ,0xFF ,0x43 ,0x5F ,0xD9 ,0xFF ,0x43 ,0x5F ,0xD9 ,0xFF ,0xBF ,0x92 ,0x6F ,0xFF ,0xBF ,0x92 ,0x6F ,0xFF ,0x43 ,0x5F ,0xD9 ,0xFF ,0x43 ,0x5F ,0xD9 ,0xFF ,0xBF ,0x92 ,0x6F ,0xFF ,0xBF ,0x92 ,0x6F ,0xFF ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,};

