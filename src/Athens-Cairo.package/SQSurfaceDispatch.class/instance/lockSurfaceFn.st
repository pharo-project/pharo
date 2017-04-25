accessing structure variables
lockSurfaceFn
	"This method was automatically generated"
	^FFICallback forAddress: ((handle pointerAt: OFFSET_LOCKSURFACEFN) asInteger)