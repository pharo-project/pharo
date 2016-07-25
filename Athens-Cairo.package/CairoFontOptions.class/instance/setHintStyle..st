hint options
setHintStyle: aStyle
	"cairo_font_options_set_hint_style ()

void                cairo_font_options_set_hint_style   (cairo_font_options_t *options,
                                                         cairo_hint_style_t hint_style);

Sets the hint style for font outlines for the font options object. This controls whether to fit font outlines to the pixel grid, and if so, whether to optimize for fidelity or contrast. See the documentation for cairo_hint_style_t for full details. "
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( 
		void cairo_font_options_set_hint_style (self, uint aStyle))

 