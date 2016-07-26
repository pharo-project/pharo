private
primCreate
"cairo_font_options_t * cairo_font_options_create        (void);

Allocates a new font options object with all options initialized to default values.

Returns :
	a newly allocated cairo_font_options_t. Free with cairo_font_options_destroy(). This function always returns a valid pointer; if memory cannot be allocated, then a special error object is returned where all operations on the object do nothing. You can check for this with cairo_font_options_status().

"	
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(CairoFontOptions cairo_font_options_create ())