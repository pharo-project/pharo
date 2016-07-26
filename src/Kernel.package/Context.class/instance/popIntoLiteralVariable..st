instruction decoding
popIntoLiteralVariable: value 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into a literal variable of my method."

	value value: self pop