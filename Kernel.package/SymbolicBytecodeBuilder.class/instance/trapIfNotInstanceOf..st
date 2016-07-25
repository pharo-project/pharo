instruction decoding
trapIfNotInstanceOf: behaviorOrArrayOfBehavior 
	"If the top of stack is not an instance of either the argument, or, if the
	argument is an Array,
	any of the elements of the argument, send the class trap message
	to the current context."
	self addBytecode: 'trapIfNotInstanceOf: ' , behaviorOrArrayOfBehavior printString
