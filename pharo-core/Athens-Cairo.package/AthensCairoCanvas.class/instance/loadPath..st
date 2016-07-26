private
loadPath: aPath

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	 
	^ self nbCall: #( void cairo_append_path (self , AthensCairoPath aPath))
