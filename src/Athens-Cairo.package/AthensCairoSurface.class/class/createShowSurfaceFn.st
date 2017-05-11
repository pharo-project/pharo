surface plugin callbacks
createShowSurfaceFn
	"
	int showSurface(sqIntptr_t handle, int x, int y, int w, int h);
		Display the contents of the surface on the actual screen.

		If ioShowSurface() is called the surface in question represents
		a Squeak DisplayScreen.

	"
	^ nil
	"^ FFICallback 
		signature: #(int (void *handle, int x, int y, int w, int h))
		block: [ :handle :x :y :w :h | 0 ""Do nothing"" ]"