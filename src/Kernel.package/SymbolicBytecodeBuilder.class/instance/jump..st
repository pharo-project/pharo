instruction decoding
jump: offset
	"Print the Unconditional Jump bytecode."

	self addBytecode: 'jumpTo: ' , (scanner pc + offset) printString.