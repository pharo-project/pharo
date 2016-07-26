instruction decoding
blockReturnTop
	"Simulate the interpreter's action when a ReturnTopOfStackToCaller bytecode is 
	 encountered in the receiver.  This should only happen in a closure activation."
	[closureOrNil isClosure] assert.
	^self return: self pop from: self