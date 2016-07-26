private
showGlyphs: glyphs size: numGlyphs

	"A drawing operator that generates the shape from a string of UTF-8 characters, rendered according to the current font_face, font_size (font_matrix), and font_options. "
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(
	
		void cairo_show_glyphs ( self ,
			void * glyphs,
			int numGlyphs) )
