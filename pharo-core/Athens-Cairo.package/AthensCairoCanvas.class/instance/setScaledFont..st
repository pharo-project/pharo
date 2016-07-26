private
setScaledFont: aFont
	"Replaces the current font face, font matrix, and font options in the cairo_t with those of the cairo_scaled_font_t. Except for some translation, the current CTM of the cairo_t should be the same as that of the cairo_scaled_font_t, which can be accessed using cairo_scaled_font_get_ctm().
 "
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( void cairo_set_scaled_font (self , CairoScaledFont aFont))
