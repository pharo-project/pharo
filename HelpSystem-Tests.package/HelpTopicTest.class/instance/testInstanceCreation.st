testing
testInstanceCreation

	|instance|
	instance := self defaultTestClass named: 'My Topic'.
	self assert: instance title = 'My Topic'.
