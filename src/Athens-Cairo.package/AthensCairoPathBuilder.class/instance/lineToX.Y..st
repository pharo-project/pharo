private
lineToX: x Y: y

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(void cairo_line_to (AthensCairoCanvas context,
		 double x, double y ) )
