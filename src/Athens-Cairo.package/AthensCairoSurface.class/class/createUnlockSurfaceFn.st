surface plugin callbacks
createUnlockSurfaceFn

	"
		int unlockSurface(int handle, int x, int y, int w, int h);
		Unlock the bits of a (possibly modified) surface after BitBlt completed.
		The return value is ignored.

		The arguments provided specify the dirty region of the surface. If the
		surface is unmodified all arguments are set to zero.

	"
	^ FFICallback 
		signature: #(int (int handle, int x, int y, int w, int h))
		block: [ :handle :x :y :w :h | 0 "Do nothing" ]