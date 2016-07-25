tests
testMessage
	| model |
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: #('a' 'b' ).
	model loadEntries.
	self assert: model notEmpty.
	self assert: model hasMessage.
	self assert: model message = 'press key for selectors'.
	model narrowWith: 'b'.
	self shouldnt: model hasMessage.
	model narrowWith: ''.
	self assert: model hasMessage.
	self assert: model message = 'press key for selectors'.
	model narrowWith: 'hagadagadu'.
	self assert: model hasMessage.
	self assert: model message = 'no completions found'.
	model := NECUntypedModel new.
	self assert: model hasMessage.
	self assert: model message = 'press key for selectors'