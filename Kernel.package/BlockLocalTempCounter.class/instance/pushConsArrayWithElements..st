instruction decoding
pushConsArrayWithElements: numElements
	"Push Cons Array of size numElements popping numElements items from the stack into the array bytecode."
	stackPointer := stackPointer - numElements + 1