accessing
signalerContext
	"Find the first sender of signal(:), the first context which is neither for an instance method nor for a class side method of Exception (or subclass).
	This will make sure that the same context is found for both, `Error signal` and `Error new signal`"

	^ signalContext findContextSuchThat: [ :context |
		(context receiver == self 
		or: [ context receiver == self class ]) not ]