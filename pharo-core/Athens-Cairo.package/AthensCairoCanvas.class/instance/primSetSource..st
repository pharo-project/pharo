private
primSetSource: aPattern
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^ self nbCall: #(void cairo_set_source ( self, AthensCairoPatternPaint aPattern))