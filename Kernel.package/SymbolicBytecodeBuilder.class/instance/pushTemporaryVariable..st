instruction decoding
pushTemporaryVariable: offset
	"Print the Push Contents Of Temporary Variable Whose Index Is the 
	argument, offset, On Top Of Stack bytecode."

	self addBytecode: 'pushTemp: ' , offset printString