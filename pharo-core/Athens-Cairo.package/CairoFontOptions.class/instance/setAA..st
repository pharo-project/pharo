antialiasing
setAA: antialias

"cairo_font_options_set_antialias ()

void                cairo_font_options_set_antialias    (cairo_font_options_t *options,
                                                         cairo_antialias_t antialias);

Sets the antialiasing mode for the font options object. This specifies the type of antialiasing to do when rendering text.

options :
	a cairo_font_options_t

antialias :
	the new antialiasing mode"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( 
	void cairo_font_options_set_antialias (self, uint antialias))
