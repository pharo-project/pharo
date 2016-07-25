object
testArrayPairs42
	| pairs |
	
	pairs := (1 to: 42) asArray gtInspectorVariableValuePairs.
	self assert: pairs size = 42.
	(pairs collect: #key) allSatisfy: [ :e | (pairs at: e) = e ].
	
	self assert: ((pairs collect: #key) = (1 to: 42) asOrderedCollection).