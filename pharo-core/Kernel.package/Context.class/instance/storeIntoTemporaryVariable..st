instruction decoding
storeIntoTemporaryVariable: offset 
	"Simulate the action of bytecode that stores the top of the stack into one 
	of my temporary variables."

	self at: offset + 1 put: self top