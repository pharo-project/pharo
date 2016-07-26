instruction decoding
storeIntoReceiverVariable: offset 
	"Print the Store Top Of Stack Into Instance Variable Of Method bytecode."

	self addBytecode: 'storeIntoRcvr: ' , offset printString