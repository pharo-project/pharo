accessing
getExtentsOf: utf8String into: extentObj 
	"void                cairo_scaled_font_extents           (cairo_scaled_font_t *scaled_font,
                                                         cairo_font_extents_t *extents);
	"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(
		void cairo_scaled_font_text_extents (self,
			void *utf8String,
			cairo_text_extents_t * extentObj))
		