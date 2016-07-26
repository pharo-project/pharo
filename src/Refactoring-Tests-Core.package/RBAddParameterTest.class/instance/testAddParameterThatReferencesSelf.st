tests
testAddParameterThatReferencesSelf
	| refactoring class |
	refactoring := RBAddParameterRefactoring 
		addParameterToMethod: ('test' , 'Foo:') asSymbol
		in: RBRefactoryTestDataApp
		newSelector: #testFoo:bar:
		initializer: 'self printString'.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #testFoo:bar:) = (RBParser parseMethod: 'testFoo: anObject bar: anObject1
								^self class + anObject').
	self assert: (class parseTreeFor: #callFoo) = (RBParser parseMethod: 'callFoo ^self testFoo: 5 bar: (self printString)').
	self deny: (class directlyDefinesMethod: ('test' , 'Foo:') asSymbol)