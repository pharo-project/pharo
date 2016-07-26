instruction decoding
storeIntoTemporaryVariable: offset 
	"Print the Store Top Of Stack Into Temporary Variable Of Method 
	bytecode."

	self addBytecode: 'storeIntoTemp: ' , offset printString