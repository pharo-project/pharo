private
markDirty

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #( void cairo_surface_mark_dirty (self) )