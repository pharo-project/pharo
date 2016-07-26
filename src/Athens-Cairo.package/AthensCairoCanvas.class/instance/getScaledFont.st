private
getScaledFont
	"Returns the current scaled font. This object is owned by cairo. To keep a reference to it, you must call cairo_scaled_font_reference().
 "
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( CairoScaledFont cairo_get_scaled_font ( self ))