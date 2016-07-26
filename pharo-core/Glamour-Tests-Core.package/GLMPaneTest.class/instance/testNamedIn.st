tests
testNamedIn
	| aPane |
	aPane := GLMPane named: 1 in: 2.
	self assert: aPane name equals: 1.
	self assert: aPane browser equals: 2.
	self assert: aPane ports isEmpty.
	self assert: aPane presentations isEmpty