tests
testMessage
	| model |
	model := NECTypedModel class: NECTestClass.
	self shouldnt: model hasMessage.
	model narrowWith: 'hagadagadu'.
	self assert: model hasMessage.
	self assert: model message = 'no completions found'