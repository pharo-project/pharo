testing
testDynamicVariableBlockReturnValue
	| returnValue |
	returnValue := TestDynamicVariable value: 10 during: [ TestDynamicVariable value + 1 ].
	self assert: returnValue equals: 11