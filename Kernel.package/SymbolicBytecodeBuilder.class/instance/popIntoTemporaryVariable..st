instruction decoding
popIntoTemporaryVariable: offset 
	"Print the Remove Top Of Stack And Store Into Temporary Variable 
	bytecode."

	self addBytecode: 'popIntoTemp: ' , offset printString