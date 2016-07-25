tests
testReferenceCycle
	| array |
	array := (self materialize: '[1,@1]').
	self assert: array class = STON listClass.
	self assert: array size = 2.
	self assert: array first = 1.
	self assert: array second == array