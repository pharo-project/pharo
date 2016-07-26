accessing
getDataPtr
	"get a pointer to surface bitmap data"

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(
		void* cairo_image_surface_get_data ( self ) )

