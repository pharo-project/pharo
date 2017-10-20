tests
testAllCallsOnASymbol

	| set cm |
	set := Object allCallsOn: #shallowCopy.
	cm := (set detect: [ :rgMethod | (rgMethod selector == #copy) and: [rgMethod className = #Object]]) compiledMethod.
	self assert: (cm literals includes: #shallowCopy)