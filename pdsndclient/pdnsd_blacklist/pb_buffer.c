/*
Simple buffer for storing requested hosts in pdnsd and print them to stdout only once
as they are requested multiple times, often 6x in a row, then requested again few seconds later

TODO:
ok - push address to buffer
ok - push every address only once!
ok - remove trailing dot from string
ok - do not depend on parsing output in external bash script and append addresses to special log automatically
*/

#define PB_SIZE 500
char * pb_buffer[PB_SIZE];
int pb_pointer = 0;

void pb_push(char * s) {
    // add one string to buffer and increment pointer
    printf("pb_push: begin\n");
    fflush(stdout);
    int i,len;
    char poms[256];
    // remove trailing dot
    strcpy(poms,s);
    len = strlen(poms);
    if (poms[len-1] == '.')
      poms[len-1] = 0;
    // first, go throught entire buffer and check if "s" isn't already in the buffer
    for (i=0; i<PB_SIZE; i++)
      if (pb_buffer[i] != NULL) 
        if (strcmp(pb_buffer[i],poms) == 0) {
          // string already in buffer, do nothing!
          printf("pb_push: end (already in buffer)\n");
          fflush(stdout);
          return;
        }
    // allocate it if necessary
    if (pb_buffer[pb_pointer] == NULL)
      pb_buffer[pb_pointer] = (char*)(malloc(256*sizeof(char)));
    // fill string into buffer
    strcpy(pb_buffer[pb_pointer],poms);
    //printf("String %s was added to buffer[%d] ",s,pb_pointer);
    // increment pointer
    pb_pointer = (pb_pointer+1) % PB_SIZE;
    // print address to stdout
    //printf("(next position will be %d)\n",pb_pointer);
    printf("%s\n",poms);
    // append address to special log of recent files
    FILE *out = fopen("/var/cache/pdnsd/recent", "a");
    fprintf(out, "%s\n", poms);
    fclose(out);
    printf("pb_push: end\n");
    fflush(stdout);
}

void pb_debug(char * note) {
  // print entire buffer to stdout
  int i;
  printf("pb_buffer: %s\n\n",note);
  for (i=0; i<PB_SIZE; i++)
    printf("  [%2d]: %s\n",i,pb_buffer[i]);
  printf("\n");
}
