object
testArrayPairs43
	| pairs |
	
	pairs := (1 to: 43) asArray gtInspectorVariableValuePairs.
	self assert: pairs size = 42.
	(pairs collect: #key) allSatisfy: [ :e | (pairs at: e) = e ].
	
	self assert: ((pairs collect: #key) = ((1 to: 21) asOrderedCollection addAll: (23 to: 43) asOrderedCollection; yourself)).