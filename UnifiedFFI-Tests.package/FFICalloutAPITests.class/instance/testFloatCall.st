tests atomic
testFloatCall
	"Test using generic FFI spec"
	| callback result |
	
	callback := FFICallback 
		signature: #(float (float x)) 
		block: [ :x | x abs ].
	FFICallbackFunctionResolution registerCallback: callback as: #FloatAbsolute.

	result := self ffiFloatAbs: 65.
	self assert: result equals: 65.0.
	
	result := self ffiFloatAbs: 65.0.
	self assert: result equals: 65.0.

	result := self ffiFloatAbs: $A.
	self assert: result equals: 65.0.

	result := self ffiFloatAbs: true.
	self assert: result equals: 1.0.

	
