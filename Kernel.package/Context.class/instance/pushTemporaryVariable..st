instruction decoding
pushTemporaryVariable: offset 
	"Simulate the action of bytecode that pushes the contents of the 
	temporary variable whose index is the argument, index, on the top of 
	the stack."

	self push: (self at: offset + 1)