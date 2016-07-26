tests atomic
testCharCall
	"Test using generic FFI spec"
	| result  |
	
	result := self ffiToLower: $A.
	self assert: result equals: $a.
	 
	result := self ffiToLower: 65.
	self assert: result equals: $a.
	
	result := self ffiToLower: 65.0.
	self assert: result equals: $a.

	result := self ffiToLower: true.
	self assert: result equals: (Character value: 1).
