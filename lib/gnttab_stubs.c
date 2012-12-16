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

#include <xenctrl.h>

#define PAGE_SHIFT		12
#define PAGE_SIZE               (1UL << PAGE_SHIFT)
#define PAGE_MASK               (~(PAGE_SIZE-1))

#define _G(__g) ((xc_gnttab *)(__g))

#define Val_none (Val_int(0))

CAMLprim value stub_xc_gnttab_open(void)
{
	CAMLparam0();
	xc_gnttab *xgh;
	xgh = xc_gnttab_open(NULL, 0);
	if (xgh == NULL)
		caml_failwith("Failed to open interface");
	CAMLreturn((value)xgh);
}

CAMLprim value stub_xc_gnttab_close(value xgh)
{
	CAMLparam1(xgh);

	xc_gnttab_close(_G(xgh));

	CAMLreturn(Val_unit);
}

CAMLprim value stub_xc_gnttab_map_grant_ref(value xgh, value domid, value ref, value prot)
{
	CAMLparam4(xgh, domid, ref, prot);
	uint32_t c_domid = Int32_val(domid);
	uint32_t c_ref = Int32_val(ref);
	int c_flags = Int_val(prot);

	void *map = xc_gnttab_map_grant_ref(_G(xgh), c_domid, c_ref, c_flags);

	if(map==NULL) {
		caml_failwith("Failed to map grant ref");
	}

	CAMLreturn(caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL, 1, map, 1 << XC_PAGE_SHIFT));
}

CAMLprim value stub_xc_gnttab_map_grant_refs(value xgh, value refs_and_domids, value prot)
{
	CAMLparam3(xgh, refs_and_domids, prot);
	CAMLreturn(Val_unit);
}

CAMLprim value stub_xc_gnttab_unmap(value xgh, value array) 
{
	CAMLparam2(xgh, array);

	int size = Caml_ba_array_val(array)->dim[0];
	int pages = size >> XC_PAGE_SHIFT;
	int result = xc_gnttab_munmap(_G(xgh), Caml_ba_data_val(array), pages);
	if(result!=0) {
		caml_failwith("Failed to unmap grant");
	}

	CAMLreturn(Val_unit);
}

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
