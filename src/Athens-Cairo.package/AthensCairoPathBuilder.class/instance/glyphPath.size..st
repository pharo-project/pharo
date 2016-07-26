private
glyphPath: glyphs size: numGlyphs
	"void cairo_glyph_path (cairo_t *cr,
                  const cairo_glyph_t *glyphs,
                  int num_glyphs);"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(
	
		void cairo_glyph_path ( AthensCairoCanvas context ,
			void * glyphs,
			int numGlyphs) )
