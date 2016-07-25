testing
testCompletionAtWithReturn
	| completion |
	completion := model completionAt: 3.
	self assert: completion = 'toBeOverridenWithReturn
	^ super toBeOverridenWithReturn'