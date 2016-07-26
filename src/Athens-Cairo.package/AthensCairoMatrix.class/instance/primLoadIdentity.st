private
primLoadIdentity
	"initialize with identity transform"
	
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>

	^ self nbCall: #( void cairo_matrix_init_identity (AthensCairoMatrix * self ) )