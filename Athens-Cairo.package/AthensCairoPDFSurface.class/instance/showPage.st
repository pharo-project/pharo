private
showPage

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	^self nbCall: #( void cairo_surface_show_page (self) )