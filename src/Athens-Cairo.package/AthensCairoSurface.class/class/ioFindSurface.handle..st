private
ioFindSurface: id handle: handlePtr
"	int ioFindSurface(int surfaceID, sqSurfaceDispatch *fn, int *surfaceHandle);
		Find the surface with the given ID, and, optionally,
		the given set of surface functions. The registered handle
		is returned in surfaceHandle. Return true if successful
		(e.g., the surface has been found), false otherwise.
"


	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	self nbCall: #( bool ioFindSurface(int id, 0 , int *handlePtr) ) module: #SurfacePlugin