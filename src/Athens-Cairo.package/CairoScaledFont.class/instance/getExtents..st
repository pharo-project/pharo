accessing
getExtents: cairoFontExtents
	"void                cairo_scaled_font_extents           (cairo_scaled_font_t *scaled_font,
                                                         cairo_font_extents_t *extents);
	"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(int cairo_scaled_font_extents (self, CairoFontExtents * cairoFontExtents ))
		