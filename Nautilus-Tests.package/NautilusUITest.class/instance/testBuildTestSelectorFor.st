tests
testBuildTestSelectorFor

	| method selector |
	method := MorphicEventHandler >> #keyStroke:fromMorph:.
	selector := nautilusUI buildTestSelectorFor: method.
	self assert: selector equals: 'testKeyStrokeFromMorph'