tests atomic
testIntCall
	"Test using generic FFI spec"
	| result |
	
	result := self ffiIntAbs: 65.
	self assert: result equals: 65.
	
	result := self ffiIntAbs: 65.0.
	self assert: result equals: 65.

	result := self ffiIntAbs: $A.
	self assert: result equals: 65.

	result := self ffiIntAbs: true.
	self assert: result equals: 1.
