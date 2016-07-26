accessing structure variables
unlockSurfaceFn: anObject
	"This method was automatically generated"
	unlockSurfaceFn := anObject. "Ensure it's not gced"
	handle unsignedLongAt: 17 put: anObject thunk address