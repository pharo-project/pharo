private
setFontSize: aSize
	"
	void                cairo_set_font_size                 (cairo_t *cr,
                                                         double size);
	"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( void cairo_set_font_size (self , double aSize ))
