tests
testMoveDefinition
	| refactoring |
	refactoring := RBMoveVariableDefinitionRefactoring 
		bindTight: (self 
				convertInterval: (19 to: 22)
				for: (RBRefactoryTestDataApp sourceCodeAt: #moveDefinition))
		in: RBRefactoryTestDataApp
		selector: #moveDefinition.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #moveDefinition) = (RBParser parseMethod: 'moveDefinition
								^(self collect: 
										[:each | 
										| temp |
										temp := each printString.
										temp , temp]) 
									select: 
										[:each | 
										| temp |
										temp := each size.
										temp odd]')