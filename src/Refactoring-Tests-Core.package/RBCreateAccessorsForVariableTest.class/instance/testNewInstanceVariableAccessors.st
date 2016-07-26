tests
testNewInstanceVariableAccessors
	| ref class |
	ref := RBCreateAccessorsForVariableRefactoring 
		variable: 'foo1'
		class: RBLintRuleTest
		classVariable: false.
	self executeRefactoring: ref.
	class := ref model classNamed: #RBLintRuleTest.
	self deny: ref changes changes isEmpty.
	self assert: ref setterMethod == #foo1:.
	self assert: ref getterMethod == #foo1.
	self assert: (class parseTreeFor: #foo1) = (RBParser parseMethod: 'foo1 ^foo1').
	self assert: (class parseTreeFor: #foo1:) = (RBParser parseMethod: 'foo1: anObject foo1 := anObject')