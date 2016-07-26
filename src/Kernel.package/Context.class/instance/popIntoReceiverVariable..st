instruction decoding
"protocol: instruction decoding"
popIntoReceiverVariable: offset 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into an instance variable of my receiver."

	self object: self receiver instVarAt: offset + 1 put: self pop