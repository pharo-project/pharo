private
primSelectFont: aFontFamily
		slant: aFontSlant
		weight: aFontWeight 
		on: aCanvas
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	self nbCall: #(void cairo_select_font_face (
			cairo_t aCanvas,
   			char * aFontFamily,
      		cairo_font_slant_t aFontSlant,
        	cairo_font_weight_t aFontWeight) )
	