accessing structure variables
getSurfaceFormatFn: anObject
	"This method was automatically generated"
	getSurfaceFormatFn := anObject. "Ensure it's not gced"
	handle unsignedLongAt: 9 put: anObject thunk address