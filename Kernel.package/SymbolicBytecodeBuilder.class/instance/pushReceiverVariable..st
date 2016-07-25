instruction decoding
pushReceiverVariable: offset
	"Print the Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."

	self addBytecode: 'pushRcvr: ' , offset printString