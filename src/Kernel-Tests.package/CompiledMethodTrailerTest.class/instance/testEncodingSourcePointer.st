tests
testEncodingSourcePointer

	| trailer |
	
	trailer := CompiledMethodTrailer new.
	
	CompiledMethod allInstancesDo: [:method | | ptr |
		trailer method: method.
		self assert: ( (ptr := method sourcePointer) == trailer sourcePointer).
		"the last bytecode index must be at 0"
		ptr ~= 0 ifTrue: [
			self assert: (method endPC = trailer endPC) ].
	 ].