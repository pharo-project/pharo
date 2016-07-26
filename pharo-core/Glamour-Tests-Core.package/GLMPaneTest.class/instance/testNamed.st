tests
testNamed
	| aPane |
	aPane := GLMPane named: 1.
	self assert: aPane name equals: 1.
	self assert: aPane browser equals: GLMNoBrowser new.
	self assert: aPane ports isEmpty.
	self assert: aPane presentations isEmpty