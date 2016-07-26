tests
testTitle
	| model |
	model := NECUntypedModel
				class: NECTestClass
				temporaries: #().
	self assert: model title isNil