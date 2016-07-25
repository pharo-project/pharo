tests
testRenameTestMethod
	| refactoring class |
	refactoring := RBRenameMethodRefactoring 
		renameMethod: ('rename' , 'ThisMethod:') asSymbol
		in: RBRefactoryTestDataApp
		to: #renameThisMethod2:
		permutation: (1 to: 1).
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #renameThisMethod2:) = (RBParser parseMethod: 'renameThisMethod2: anArg
	^self').
	self assert: (class parseTreeFor: #callMethod) = (RBParser parseMethod: 'callMethod
	^(self renameThisMethod2: 5)').
	self assert: (class parseTreeFor: #symbolReference) = (RBParser parseMethod: 'symbolReference
		^ #(#renameThisMethod2: #(4 #renameThisMethod2:))').
	self deny: (class directlyDefinesMethod: ('rename' , 'ThisMethod:') asSymbol)