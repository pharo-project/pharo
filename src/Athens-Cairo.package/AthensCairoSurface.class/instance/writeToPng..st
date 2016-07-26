converting
writeToPng: aFileName
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	self nbCall: #(void cairo_surface_write_to_png (self, String aFileName) )
