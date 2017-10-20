bytecode decoding
stackDeltaForPrimitive: primitiveIndex in: method 
	"This is the default implementation. Subclasses with inline primitives
	will need to override."
	^ 0