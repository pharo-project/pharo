tests
testAddParameterThatReferencesModelGlobal
	| refactoring class |
	refactoring := RBAddParameterRefactoring 
		model: model
		addParameterToMethod: ('test' , 'Foo:') asSymbol
		in: RBRefactoryTestDataApp
		newSelector: #testFoo:bar:
		initializer: 'Bar new'.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #testFoo:bar:) = (RBParser parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self assert: (class parseTreeFor: #callFoo) = (RBParser parseMethod: 'callFoo ^self testFoo: 5 bar: (Bar new)').
	self deny: (class directlyDefinesMethod: ('test' , 'Foo:') asSymbol)