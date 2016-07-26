tests
testExtractToTemporaryInsideBlock
	| refactoring |
	refactoring := RBExtractToTemporaryRefactoring 
		extract: (self 
				convertInterval: (133 to: 141)
				for: (RBRefactoryTestDataApp sourceCodeAt: #noMoveDefinition))
		to: 'asdf'
		from: #noMoveDefinition
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #noMoveDefinition) = (RBParser parseMethod: 'noMoveDefinition
	| temp |
	^(self collect: 
			[:each | 
			temp := each printString.
			temp , temp]) 
		select: [:each | | asdf | asdf := each size. temp := asdf + temp]')