tests - special cases
testNoRemoteBlockArgument
	| ast assignment vars |
	ast := (OCOpalExamples>>#noRemoteBlockArgument) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	
	self deny: (ast scope lookupVar: 'block') isRemote .
	self deny: (ast scope lookupVar: 'block1') isRemote.
	self deny: (ast scope lookupVar: 'block2') isRemote.


