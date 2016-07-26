tests
testAddParameterForTwoArgumentMessage
	| refactoring class |
	refactoring := RBAddParameterRefactoring 
		addParameterToMethod: ('called:' , 'on:') asSymbol
		in: RBRefactoryTestDataApp
		newSelector: #called:bar:on:
		initializer: '#(1.0)'.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #called:bar:on:) = (RBParser parseMethod: 'called: anObject bar: aBlock on: anObject1
							Transcript
								show: anObject printString;
								cr.
								aBlock value').
	self assert: (class parseTreeFor: #caller) = (RBParser parseMethod: 'caller
							| anObject |
							anObject := 5.
							self 
								called: anObject + 1
								bar: [^anObject] on: #(1.0)').
	self deny: (class directlyDefinesMethod: ('called:' , 'on:') asSymbol)