surface plugin callbacks
createGetSurfaceFormatFn
	"
		int getSurfaceFormat(sqIntptr_t handle, int* width, int* height, int* depth, int* isMSB);

		Return general information about the OS drawing surface.
		Return true if successful, false otherwise.

		The returned values describe the basic properties such as
		width, height, depth and LSB vs. MSB pixels.
	"
	^ FFICallback 
		signature: #(int (void *handle, int* width, int* height, int* depth, int* isMSB))
		block: [ :handle :width :height :depth :isMSB |
			[width signedLongAt: 1 put: (self get_width: handle).
			 height signedLongAt: 1 put: (self get_height: handle).
			 depth signedLongAt: 1 put: 32.
			 isMSB signedLongAt: 1 put: 0. 
			 1 "Everything ok"] 
			on: Error do: [ :e | 0 "Error happened" ]]