testing
testDisplayLabel
	|instance|
	instance := self defaultTestClass with: (HelpTopic named: 'My Topic').
	self assert: instance asString = 'My Topic'
	