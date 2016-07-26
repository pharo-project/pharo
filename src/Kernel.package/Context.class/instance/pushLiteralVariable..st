instruction decoding
pushLiteralVariable: value 
	"Simulate the action of bytecode that pushes the contents of the literal 
	variable whose index is the argument, index, on the top of the stack."

	self push: value value