instruction decoding
popIntoReceiverVariable: offset 
	"Print the Remove Top Of Stack And Store Into Instance Variable 
	bytecode."

	self addBytecode: 'popIntoRcvr: ' , offset printString