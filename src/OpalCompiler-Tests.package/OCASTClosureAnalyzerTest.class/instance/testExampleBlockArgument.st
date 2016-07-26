tests - blocks
testExampleBlockArgument
	| ast assignment vars blockScope blockScope2 |
	ast := (OCOpalExamples>>#exampleBlockArgument) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	self assert: ast scope tempVector size = 0.
	self assert: ast scope copiedVars size = 0.
	
	self deny: (ast scope lookupVar: 'block') isRemote. 
	self deny: (ast scope lookupVar: 'block1') isRemote. 
	self deny: (ast scope lookupVar: 'block2') isRemote. 
	
	blockScope := (OCScopesCollector new visitNode: ast) scopes second.
	
	self assert: blockScope tempVars size = 2.
	self assert: blockScope tempVector size = 0.
	self assert: blockScope copiedVars size = 1.
	self deny: (blockScope lookupVar: 'temp') isRemote.
	self assert: (blockScope lookupVar: 'temp') isEscapingRead.
	self assert: (blockScope lookupVar: 'temp') isWrite.
	self deny: (blockScope lookupVar: 'temp') isEscapingWrite.
	self deny: (blockScope lookupVar: 'arg') isRemote.
	
	
	blockScope2 := (OCScopesCollector new visitNode: ast) scopes third.
	self assert: blockScope2 tempVars size = 0.
	self assert: blockScope2 tempVector size = 0.
	self assert: blockScope2 copiedVars size = 1.
	
	
	
	


