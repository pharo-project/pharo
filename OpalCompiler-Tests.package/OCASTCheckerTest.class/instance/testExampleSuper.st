testing - variables
testExampleSuper
	| ast assignment vars |
	ast := (OCOpalExamples>>#exampleSuper) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	self assert: (ast scope lookupVar: 'super') isSuper.
	
	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 
	self assert: assignment value binding isSuper.