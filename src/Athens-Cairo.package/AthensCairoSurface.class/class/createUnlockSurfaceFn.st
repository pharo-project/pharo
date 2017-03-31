surface plugin callbacks
createUnlockSurfaceFn
	"
		int unlockSurface(sqIntptr_t handle, int x, int y, int w, int h);
		Unlock the bits of a (possibly modified) surface after BitBlt completed.
		The return value is ignored.

		The arguments provided specify the dirty region of the surface. If the
		surface is unmodified all arguments are set to zero.

	"
	^ nil
	"^ FFICallback 
		signature: #(int (void *handle, int x, int y, int w, int h))
		block: [ :handle :x :y :w :h | 0 ""Do nothing"" ]"