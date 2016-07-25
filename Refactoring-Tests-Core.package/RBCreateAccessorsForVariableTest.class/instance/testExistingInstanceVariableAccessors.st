tests
testExistingInstanceVariableAccessors
	| ref |
	ref := RBCreateAccessorsForVariableRefactoring 
		variable: 'name'
		class: RBLintRuleTest
		classVariable: false.
	self executeRefactoring: ref.
	self assert: ref changes changes isEmpty.
	self assert: ref setterMethod == #name:.
	self assert: ref getterMethod == #name