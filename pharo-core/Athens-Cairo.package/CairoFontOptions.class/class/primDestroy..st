private
primDestroy: anOptions
"void                cairo_font_options_destroy          (cairo_font_options_t *options);

Destroys a cairo_font_options_t object created with cairo_font_options_create() or cairo_font_options_copy().

options :
	a cairo_font_options_t"

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(void cairo_font_options_destroy (size_t anOptions))