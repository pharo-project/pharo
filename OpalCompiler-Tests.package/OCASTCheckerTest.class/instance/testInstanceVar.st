testing - variables
testInstanceVar
	| ast assignment vars |
	ast := (OCOpalExamples>>#exampleiVar) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars isEmpty.
	
	self assert: ast scope outerScope isInstanceScope.
	self assert: (ast scope outerScope lookupVar: 'iVar') isInstance.
	
	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 
	self assert: assignment variable isInstance.