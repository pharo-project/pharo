private
setFontMatrix: aMatrix
	"
void                cairo_set_font_matrix               (cairo_t *cr,
                                                         const cairo_matrix_t *matrix);
	"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( void cairo_set_font_matrix (self , AthensCairoMatrix  * aMatrix ))
