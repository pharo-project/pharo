private
convertString: utf8String len: strlen ofFont: aScaledFont toGlyphs: glyphs numGlyphs: numGlyphs x: x y: y
"
all of this for using 
http://www.cairographics.org/manual/cairo-User-Fonts.html#cairo-user-scaled-font-text-to-glyphs-func-t

"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^self nbCall: #(
		cairo_status_t cairo_scaled_font_text_to_glyphs (CairoScaledFont aScaledFont,
			double x,
			double y,
			void * utf8String,
			int strlen,
			void ** glyphs,
			int * numGlyphs,
			0,
			0,
			0))