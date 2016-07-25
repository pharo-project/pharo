tests
testExtractToTemporaryWithDuplicates
	| refactoring |
	refactoring := RBExtractToTemporaryRefactoring 
		extract: (self 
				convertInterval: (73 to: 77)
				for: (RBRefactoryTestDataApp sourceCodeAt: #demoMethodWithDuplicates))
		to: 'temp'
		from: #demoMethodWithDuplicates
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #demoMethodWithDuplicates) = (RBParser parseMethod: 'demoMethodWithDuplicates
	| a b result1 result2 answer temp |
	a := 3.
	temp := a + 5.
	b := temp.
	result1 := a + b.
	result2 := temp.
	answer := result1 + result2 + temp.
	^ answer')