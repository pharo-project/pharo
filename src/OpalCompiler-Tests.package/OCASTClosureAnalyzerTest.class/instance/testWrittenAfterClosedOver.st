tests - special cases
testWrittenAfterClosedOver
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#writtenAfterClosedOver) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
	self assert: ast scope tempVector size = 1.
	
	self assert: (ast scope lookupVar: 'a') isRemote.

	

	
	


