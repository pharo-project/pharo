tests - blocks
testExampleWhileNoModification
"A block in an optimized loop with no modification on temp vars in the loop
only needs to copy the tempvars. No write -> no indirection vector."
	| ast |
	ast := (OCOpalExamples>>#exampleWhileNoModification) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	self assert: ast scope tempVector size = 0.
	self assert: ast scope copiedVars size = 3.
	self deny: (ast scope lookupVar: 'a') isRemote. 
	self deny: (ast scope lookupVar: 'b') isRemote. 
	self deny: (ast scope lookupVar: 'c') isRemote. 
	

	


