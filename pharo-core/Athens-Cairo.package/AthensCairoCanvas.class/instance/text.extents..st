private
text: utf8String extents: extentsObj 
"cairo_text_extents ()

void                cairo_text_extents                  (cairo_t *cr,
                                                         const char *utf8,
                                                         cairo_text_extents_t *extents);
"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>

	^ self nbCall: #( void cairo_text_extents (self, char * utf8String, cairo_text_extents_t * extentsObj) )
	