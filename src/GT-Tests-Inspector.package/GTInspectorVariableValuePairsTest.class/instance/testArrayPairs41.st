object
testArrayPairs41
	| pairs |
	
	pairs := (1 to: 41) asArray gtInspectorVariableValuePairs.
	self assert: pairs size = 41.
	(pairs collect: #key) allSatisfy: [ :e | (pairs at: e) = e ].
	
	self assert: ((pairs collect: #key) = (1 to: 41) asOrderedCollection).