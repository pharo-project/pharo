testing - variables
testExampleThisContext 
	| ast assignment vars |
	ast := (OCOpalExamples>>#exampleThisContext) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	self assert: (ast scope lookupVar: 'thisContext') isThisContext.
	
	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 
	self assert: assignment value binding isThisContext.