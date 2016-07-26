private
setPathMatrix
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_set_matrix (self,
			AthensCairoMatrix * pathTransform) )
