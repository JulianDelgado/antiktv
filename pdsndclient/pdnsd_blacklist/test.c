/*
Standalone test for pd_buffer.c
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pb_buffer.c"

int main(int argc, char **argv)
{
  // sample values
  pb_push("it.jobs.cz.");
  pb_push("it.jobs.cz.");
  pb_push("it.jobs.cz.");
  pb_push("dactyl.sourceforge.net.");
  pb_push("jvgs.sourceforge.net.");
  pb_push("kernelnewbies.org.");
  pb_push("twitter.com.");
  pb_push("kde.org.");
  pb_push("www.64bit.eu.");
  pb_push("wiki.scribus.net.");
  pb_push("www.abcprace.cz.");
  pb_push("www.boingboing.net.");
  pb_push("www.facebook.com.");
  pb_push("www.hdmag.cz.");
  pb_push("www.kde.org.");
  pb_push("www.jobs.cz.");
  pb_push("www.kernel.org.");
  
	return 0;
}

