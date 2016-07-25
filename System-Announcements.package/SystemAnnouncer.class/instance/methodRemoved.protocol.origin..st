triggering
methodRemoved: aMethod protocol: protocol origin: aBehavior
	"A method with the given selector was removed from the class."

	self announce: (MethodRemoved methodRemoved: aMethod protocol: protocol origin: aBehavior)