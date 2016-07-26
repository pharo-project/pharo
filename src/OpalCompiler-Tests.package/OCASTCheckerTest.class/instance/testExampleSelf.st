testing - variables
testExampleSelf
	| ast assignment vars |
	ast := (OCOpalExamples>>#exampleSelf) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	self assert: (ast scope lookupVar: 'self') isSelf.
	
	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 
	self assert: assignment value binding isSelf.