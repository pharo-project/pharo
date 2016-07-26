instruction decoding
callPrimitive: primNumber
	"Print the callPrimitive bytecode."
	| isInlined |
	isInlined := primNumber >= (1 << 15) ifTrue: [ 'Inlined' ] ifFalse: [ '' ].
	self addBytecode: 'call' , isInlined , 'Primitive: ' , primNumber printString