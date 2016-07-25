instruction decoding
methodReturnConstant: value 
	"Print the Return Constant bytecode."

	self addBytecode: 'return: ' , value printString