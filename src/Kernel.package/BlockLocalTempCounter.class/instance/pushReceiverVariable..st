instruction decoding
pushReceiverVariable: offset
	"Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."
	stackPointer := stackPointer + 1