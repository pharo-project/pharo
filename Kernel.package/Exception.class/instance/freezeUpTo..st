handling
freezeUpTo:  aContext
	"freeze the signal context up to the given context so the exception is usable outside the catch block"
	signalContext := signalContext copyTo: aContext