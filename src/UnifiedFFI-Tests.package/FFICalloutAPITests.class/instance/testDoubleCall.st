tests atomic
testDoubleCall
	"Test using generic FFI spec"
	| callback result |
	
	callback := FFICallback 
		signature: #(double (double x)) 
		block: [ :x | x abs ].
	FFICallbackFunctionResolution registerCallback: callback as: #DoubleAbsolute.
	
	result := self ffiDoubleAbs: 65.
	self assert: result equals: 65.0.
	
	result := self ffiDoubleAbs: 65.0.
	self assert: result equals: 65.0.

	result := self ffiDoubleAbs: $A.
	self assert: result equals: 65.0.

	result := self ffiDoubleAbs: true.
	self assert: result equals: 1.0.
