initialize
factory: block
	"Set the factory to compute values from keys to block.
	The factory will be evaluated for each key not present.
	Only my #at: message will use the factory."
	
	factory := block