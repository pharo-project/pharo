instruction decoding
pushConstant: value 
	"Simulate the action of bytecode that pushes the constant, value, on the 
	top of the stack."

	self push: value