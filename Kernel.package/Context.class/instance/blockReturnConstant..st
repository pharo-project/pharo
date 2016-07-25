instruction decoding
blockReturnConstant: value 
	"Simulate the interpreter's action when a ReturnConstantToCaller
	bytecode is 
	encountered in the receiver. This should only happen in a closure
	activation. "
	self assert: closureOrNil isClosure.
	^ self return: value from: self
