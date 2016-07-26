primitive failure
primitiveFailed: selector
	"Announce that a primitive has failed and there is no appropriate Smalltalk code to run."
	
	PrimitiveFailed signalFor: selector