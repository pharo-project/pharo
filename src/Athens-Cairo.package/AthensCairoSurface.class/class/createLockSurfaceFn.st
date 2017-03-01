surface plugin callbacks
createLockSurfaceFn
	"
	int lockSurface(sqIntptr_t handle, int *pitch, int x, int y, int w, int h);
		Lock the bits of the surface.
		Return a pointer to the actual surface bits, or NULL on failure.
		If successful, store the pitch of the surface (e.g., the bytes
		per scan line).

		For equal source/dest handles only one locking operation is performed.
		This is to prevent locking of overlapping areas which does not work with
		certain APIs (e.g., DirectDraw prevents locking of overlapping areas). 
		A special case for non-overlapping but equal source/dest handle would 
		be possible but we would have to transfer this information over to 
		unlockSurfaces somehow (currently, only one unlock operation is 
		performed for equal source and dest handles). Also, this would require
		a change in the notion of ioLockSurface() which is right now interpreted
		as a hint and not as a requirement to lock only the specific portion of
		the surface.

		The arguments in ioLockSurface() provide the implementation with
		an explicit hint what area is affected. It can be very useful to
		know the max. affected area beforehand if getting the bits requires expensive
		copy operations (e.g., like a roundtrip to the X server or a glReadPixel op).
		However, the returned pointer *MUST* point to the virtual origin of the surface
		and not to the beginning of the rectangle. The promise made by BitBlt
		is to never access data outside the given rectangle (aligned to 4byte boundaries!)
		so it is okay to return a pointer to the virtual origin that is actually outside
		the valid memory area.

		The area provided in ioLockSurface() is already clipped (e.g., it will always
		be inside the source and dest boundingBox) but it is not aligned to word boundaries
		yet. It is up to the support code to compute accurate alignment if necessary.
	"

	^ FFICallback 
		signature: #(void * (void *handle, int *pitch, int x, int y, int w, int h))
		block: [ :handle :pitch :x :y :w :h |
			pitch signedLongAt: 1 put: (self get_stride: handle).
			self get_data: handle ]