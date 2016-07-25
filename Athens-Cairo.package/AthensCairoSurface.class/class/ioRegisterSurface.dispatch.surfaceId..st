private
ioRegisterSurface: aCairoSurfaceHandle dispatch: sqSurfaceDispatchPtr surfaceId: idHolder
"
	int ioRegisterSurface(int surfaceHandle, sqSurfaceDispatch *fn, int *surfaceID);

	Register a new surface with the given handle and
	the set of surface functions. The new ID is returned
	in surfaceID. Returns true if successful, false 
	otherwise.

"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin>

	self nbCall: #(bool ioRegisterSurface(int aCairoSurfaceHandle, void *sqSurfaceDispatchPtr, void *idHolder)) module: #SurfacePlugin