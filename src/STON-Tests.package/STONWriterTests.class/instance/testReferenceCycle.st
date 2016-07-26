tests
testReferenceCycle
	| array |
	array := STON listClass with: 1 with: nil.
	array at: 2 put: array.
	self assert: (self serialize: array) = '[1,@1]'.