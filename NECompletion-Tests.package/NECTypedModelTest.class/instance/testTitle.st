tests
testTitle
	| model |
	model := NECTypedModel class: NECTestClass.
	self assert: #NECTestClass = model title