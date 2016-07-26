triggering
methodAdded: aMethod 
	"A method with the given selector was added to aClass, but not put in a protocol."

	self announce: (MethodAdded method: aMethod)