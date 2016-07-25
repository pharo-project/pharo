accessing
getExtentsOfGlyphs: glyphs ofLength: num into: extents 
	"void
	cairo_scaled_font_glyph_extents (cairo_scaled_font_t *scaled_font,
                                 const cairo_glyph_t *glyphs,
                                 int num_glyphs,
                                 cairo_text_extents_t *extents);"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(
		void cairo_scaled_font_glyph_extents (self,
                                            void *glyphs,
                                            int num,
                                            cairo_text_extents_t *extents))
		