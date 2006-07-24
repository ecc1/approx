/* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License */

#include <stdlib.h>
#include <unistd.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

static int
ifaddr(char *name, /* OUT */ struct in_addr *addr)
{
    int s = socket(PF_INET, SOCK_STREAM, 0);
    if (s == -1)
	return 0;
    struct ifreq r;
    int i;
    for (i = 0; i < sizeof(r.ifr_name); i++) {
	if ((r.ifr_name[i] = name[i]) == '\0')
	    break;
    }
    i = ioctl(s, SIOCGIFADDR, &r);
    close(s);
    if (i == 0 && r.ifr_addr.sa_family == AF_INET) {
	*addr = ((struct sockaddr_in *)&r.ifr_addr)->sin_addr;
	return 1;
    } else {
	return 0;
    }
}

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value
inet_addr_of_interface(value name)
{
    CAMLparam1(name);
    struct in_addr addr;
    if (ifaddr(String_val(name), &addr))
	CAMLreturn(alloc_inet_addr(&addr));
    else
	raise_not_found();
}