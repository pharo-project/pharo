tests
testNewClassVariableAccessors
	| ref class |
	ref := RBCreateAccessorsForVariableRefactoring 
		variable: 'Foo1'
		class: RBLintRuleTest
		classVariable: true.
	self executeRefactoring: ref.
	class := ref model metaclassNamed: #RBLintRuleTest.
	self deny: ref changes changes isEmpty.
	self assert: ref setterMethod == #foo1:.
	self assert: ref getterMethod == #foo1.
	self assert: (class parseTreeFor: #foo1) = (RBParser parseMethod: 'foo1 ^Foo1').
	self assert: (class parseTreeFor: #foo1:) = (RBParser parseMethod: 'foo1: anObject Foo1 := anObject')