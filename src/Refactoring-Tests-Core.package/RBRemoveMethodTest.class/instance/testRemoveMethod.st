tests
testRemoveMethod
	| refactoring selectors |
	selectors := Array with: ('selectorNot' , 'Referenced') asSymbol.
	refactoring := RBRemoveMethodRefactoring 
		removeMethods: selectors
		from: RBRefactoryTestDataApp.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) directlyDefinesMethod: selectors first).
	self executeRefactoring: refactoring.
	self deny: ((refactoring model classNamed: #RBRefactoryTestDataApp) directlyDefinesMethod: selectors first)