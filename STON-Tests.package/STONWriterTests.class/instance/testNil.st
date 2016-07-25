tests
testNil
	self assert: (self serialize: nil) = 'nil'