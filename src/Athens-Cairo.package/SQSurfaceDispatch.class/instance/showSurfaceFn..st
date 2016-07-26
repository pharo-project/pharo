accessing structure variables
showSurfaceFn: anObject
	"This method was automatically generated"
	showSurfaceFn := anObject. "Ensure it's not gced"
	handle unsignedLongAt: 21 put: anObject thunk address