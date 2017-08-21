tests
testFirstActionSecondAction
	| action1 action2 |
	action1 := false.
	action2 := false.
	testedInstance firstAction: [ action1 := true ] secondAction: [ action2 := true ].
	testedInstance firstButton performAction.
	testedInstance secondButton performAction.
	self assert: action1.
	self assert: action2