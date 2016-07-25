private
moveToX: x Y: y
	" move command always starts a new contour "

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(void cairo_move_to (AthensCairoCanvas context,
		 double x, double y ) )
