/*
 * Copyright (C) 2012-2013 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */

#include <stdlib.h>
#include <errno.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/bigarray.h>

#include <sys/mman.h>
#include <stdint.h>
#include <string.h>

#define PAGE_SHIFT              12
#define PAGE_SIZE               (1UL << PAGE_SHIFT)
#define PAGE_MASK               (~(PAGE_SIZE-1))

/* Blit from a string to a page */
CAMLprim value stub_xc_gnttab_string_blit(value src, value srcoff, value dst, value dstoff, value len)
{
	CAMLparam5(src,srcoff,dst,dstoff,len);

	if(Int_val(dstoff)+Int_val(len) > PAGE_SIZE) {
		caml_failwith("xxBlit exceeds page boundary");
	}
	
	if(caml_string_length(src) - Int_val(srcoff) > Int_val(len)) {
		caml_failwith("Blit overruns end of string");
	}

	char *str = String_val(src);
	char *page = Caml_ba_data_val(dst);

	memcpy(page+Int_val(dstoff),str+Int_val(srcoff),Int_val(len));
	
	CAMLreturn(Val_unit);
}

/* Blit from a page to a string */
CAMLprim value stub_xc_gnttab_ring_blit(value src, value srcoff, value dst, value dstoff, value len)
{
	CAMLparam5(src,srcoff,dst,dstoff,len);

	if(Int_val(srcoff)+Int_val(len) > PAGE_SIZE) {
		caml_failwith("Blit exceeds page boundary");
	}
	
	if(caml_string_length(dst) - Int_val(dstoff) > Int_val(len)) {
		caml_failwith("Blit overruns end of string");
	}

	char *str = String_val(dst);
	char *page = Caml_ba_data_val(src);

	memcpy(str+Int_val(dstoff),
		   page+Int_val(srcoff),
		   Int_val(len));
	
	CAMLreturn(Val_unit);
}
