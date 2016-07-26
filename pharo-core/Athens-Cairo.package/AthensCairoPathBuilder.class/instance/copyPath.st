private
copyPath 


	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #( AthensCairoPath cairo_copy_path (AthensCairoCanvas context) )
