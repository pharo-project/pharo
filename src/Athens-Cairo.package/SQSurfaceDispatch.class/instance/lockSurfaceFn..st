accessing structure variables
lockSurfaceFn: anObject
	"This method was automatically generated"
	lookSurfaceFn := anObject. "Ensure it's not gced"
	handle unsignedLongAt: 13 put: anObject thunk address