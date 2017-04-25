accessing structure variables
unlockSurfaceFn
	"This method was automatically generated"
	^FFICallback forAddress: ((handle pointerAt: OFFSET_UNLOCKSURFACEFN) asInteger)