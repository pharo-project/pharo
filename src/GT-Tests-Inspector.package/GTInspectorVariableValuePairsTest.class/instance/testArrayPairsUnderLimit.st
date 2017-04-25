object
testArrayPairsUnderLimit
	| limit pairs|
	
	limit := 2*GTInspector indexableDisplayLimit - 1.
	pairs := (1 to: limit) asArray gtInspectorVariableValuePairs.
	self assert: pairs size = limit.
	(pairs collect: #key) allSatisfy: [ :e | (pairs at: e) = e ].
	
	self assert: ((pairs collect: #key) = (1 to: limit) asOrderedCollection).