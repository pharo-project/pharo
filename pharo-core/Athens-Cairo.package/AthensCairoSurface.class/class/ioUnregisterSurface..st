private
ioUnregisterSurface: aCairoSurfaceId
"
	ioUnregisterSurface:
	Unregister the surface with the given id.
	Returns true if successful, false otherwise. 

"

	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>
	
	self nbCall: #( int ioUnregisterSurface(ulong aCairoSurfaceId) ) module: #SurfacePlugin