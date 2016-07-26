private
freeGlyphs: glyphs
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #( void cairo_glyph_free (void *glyphs))