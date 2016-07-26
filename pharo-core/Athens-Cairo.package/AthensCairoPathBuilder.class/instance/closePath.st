private
closePath
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	self nbCall: #(void cairo_close_path (AthensCairoCanvas context))