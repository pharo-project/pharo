tests
testRemoveParameter
	| refactoring class |
	refactoring := RBRemoveParameterRefactoring 
		removeParameter: 'anArg'
		in: RBRefactoryTestDataApp
		selector: ('rename' , 'ThisMethod:') asSymbol.
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #renameThisMethod) = (RBParser parseMethod: 'renameThisMethod
								^self').
	self assert: (class parseTreeFor: #callMethod) = (RBParser parseMethod: 'callMethod
								^(self renameThisMethod)').
	self deny: (class directlyDefinesMethod: ('rename' , 'ThisMethod:') asSymbol)