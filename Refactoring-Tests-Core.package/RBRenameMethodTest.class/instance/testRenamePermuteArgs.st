tests
testRenamePermuteArgs
	| refactoring class |
	refactoring := RBRenameMethodRefactoring 
		renameMethod: ('rename:' , 'two:') asSymbol
		in: RBRefactoryTestDataApp
		to: ('rename:' , 'two:') asSymbol
		permutation: #(2 1 ).
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: ('rename:' , 'two:') asSymbol) = (RBParser parseMethod: 'rename: argumentMethod two: this ^self printString, this, argumentMethod').
	self assert: (class parseTreeFor: #exampleCall) = (RBParser parseMethod: 'exampleCall ^self rename: 2 two: 1')