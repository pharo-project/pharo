private
setPathMatrix: aMatrix
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_set_matrix (self,
			AthensCairoMatrix * aMatrix) )
