tests
testAllCallsOnASymbol

	| set cm |
	set := Object allCallsOn: #shallowCopy.
	cm := (set detect: [ :rgMethod | rgMethod selector == #copy ]) compiledMethod.
	self assert: (cm methodClass == Object).
	self assert:  (cm literals includes: #shallowCopy)