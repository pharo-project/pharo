tests
testSplitCascadeRefactoring
	| refactoring |
	refactoring := RBSplitCascadeRefactoring split: (self 
				convertInterval: (54 to: 55)
				for: (self class sourceCodeAt: #methodWithCascades))
		from: #methodWithCascades
		in: self class.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBSplitCascadeRefactoringTest) parseTreeFor: #methodWithCascades) equals: (RBParser parseMethod: 'methodWithCascades
	| a receiver |
	receiver := Object new.
	receiver initialize.
	a := receiver asString.
	^ a')