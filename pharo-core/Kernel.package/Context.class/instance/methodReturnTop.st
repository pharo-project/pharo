instruction decoding
methodReturnTop
	"Simulate the action of a 'return top of stack' bytecode. This corresponds
	 to source expressions like '^something'."

	^self return: self pop from: self methodReturnContext