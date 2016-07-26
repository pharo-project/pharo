testing
testCompletionAt
	| completion |
	completion := model completionAt: 2.
	self assert: completion = 'toBeOverriden: anArgument
	super toBeOverriden: anArgument'