private
primImageFromData: data width: width height: height pitch: stride
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #(AthensCairoSurface cairo_image_surface_create_for_data (
		void *data,
		CAIRO_FORMAT_ARGB32,
		int width,
		int height,
		int stride) )

