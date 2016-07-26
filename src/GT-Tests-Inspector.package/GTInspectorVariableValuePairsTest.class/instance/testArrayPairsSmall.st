object
testArrayPairsSmall
	| pairs |
	
	pairs := (1 to: 10) asArray gtInspectorVariableValuePairs.
	self assert: pairs size = 10.
	(pairs collect: #key) allSatisfy: [ :e | (pairs at: e) = e ].