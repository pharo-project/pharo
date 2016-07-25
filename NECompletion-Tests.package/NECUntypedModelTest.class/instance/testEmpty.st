tests
testEmpty
	| model |
	model := NECUntypedModel new.
	self assert: model isEmpty.
	self assert: model entries isEmpty.
	self assert: model entryCount == 0