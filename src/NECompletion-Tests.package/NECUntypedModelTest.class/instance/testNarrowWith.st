tests
testNarrowWith
	| count model |
	model := NECUntypedModel new.
	model listLimit: Float infinity.
	
	self assert: model isEmpty.
	
	model narrowWith: 'b'.
	count := model entryCount.
	self assert: count == model entries size.
	self assert: model isEmpty not.
	
	model narrowWith: 'bar'.
	self assert: count > model entryCount.
	
	model narrowWith: 'barXXXX'.
	self assert: model isEmpty.
	
	model narrowWith: 'b'.
	self assert: count == model entryCount.
	
	model narrowWith: 'save'.
	self assert: model isEmpty not.
	
	model narrowWith: ''.
	self assert: model isEmpty