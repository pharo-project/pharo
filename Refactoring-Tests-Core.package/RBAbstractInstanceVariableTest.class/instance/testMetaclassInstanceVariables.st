tests
testMetaclassInstanceVariables
	| refactoring class |
	class := model metaclassNamed: #Foo.
	class addInstanceVariable: 'foo'.
	class compile: 'zzz ^foo := foo + foo * 2' classified: #(#testing).
	refactoring := RBAbstractInstanceVariableRefactoring 
				model: model
				variable: 'foo'
				class: class.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #foo1) 
				= (RBParser parseMethod: 'foo1
						^foo').
	self assert: (class parseTreeFor: #foo:) 
				= (RBParser parseMethod: 'foo: anObject
						^foo := anObject').
	self assert: (class parseTreeFor: #zzz) 
				= (RBParser parseMethod: 'zzz ^self foo: self foo1 + self foo1 * 2')