instruction decoding
jump: offset if: condition 
	"Print the Conditional Jump bytecode."

	self addBytecode: 
		(condition
			ifTrue: ['jumpTrue: ']
			ifFalse: ['jumpFalse: '])
			, (scanner pc + offset) printString