compiling
acceptsLoggingOfCompilation
	"Dont log sources for my subclasses, so as not to waste time
	and space storing printString versions of the string literals."

	^ self == FontSet