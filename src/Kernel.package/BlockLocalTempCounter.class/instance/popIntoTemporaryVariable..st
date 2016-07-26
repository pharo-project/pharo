instruction decoding
popIntoTemporaryVariable: offset 
	"Remove Top Of Stack And Store Into Temporary Variable bytecode."
	stackPointer := stackPointer - 1