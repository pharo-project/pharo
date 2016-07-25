tests
testBoolean
	self assert: (self serialize: true) = 'true'.
	self assert: (self serialize: false) = 'false'