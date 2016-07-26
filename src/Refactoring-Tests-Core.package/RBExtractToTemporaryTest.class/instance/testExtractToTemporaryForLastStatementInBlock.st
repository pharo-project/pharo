tests
testExtractToTemporaryForLastStatementInBlock
	| refactoring |
	refactoring := RBExtractToTemporaryRefactoring 
		extract: (self 
				convertInterval: (52 to: 73)
				for: (RBRefactoryTestDataApp sourceCodeAt: #caller2))
		to: 'temp'
		from: #caller2
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller2) = (RBParser parseMethod: 'caller2
	^(1 to: 10) inject: 1 into: [:sum :each | | temp | temp := sum * (self foo: each). temp]')