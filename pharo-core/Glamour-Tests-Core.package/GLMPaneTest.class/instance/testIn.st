tests
testIn
	| aPane |
	aPane := GLMPane in: 1.
	self assert: aPane name equals: aPane defaultName.
	self assert: aPane browser equals: 1.
	self assert: aPane ports isEmpty.
	self assert: aPane presentations isEmpty