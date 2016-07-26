instruction decoding
"protocol: instruction decoding"
storeIntoReceiverVariable: offset 
	"Simulate the action of bytecode that stores the top of the stack into an 
	instance variable of my receiver."

	self object: self receiver instVarAt: offset + 1 put: self top