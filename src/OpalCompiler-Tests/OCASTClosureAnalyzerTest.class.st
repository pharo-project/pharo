Class {
	#name : #OCASTClosureAnalyzerTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'testing - variables' }
OCASTClosureAnalyzerTest >> testBlockArgumentIsArgumentVariable [
	| ast blockNode |
	ast := (OCOpalExamples>>#exampleForBlockArgument) parseTree.
	ast doSemanticAnalysis.
	blockNode := ast body statements first value.
	self assert: blockNode arguments notEmpty.
	self assert: blockNode arguments first isArgument
	
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testDoubleRemoteAnidatedBlocks [
	| ast scopes |
	ast := (OCOpalExamples >> #doubleRemoteAnidatedBlocks) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 2.

	scopes := (OCScopesCollector new visitNode: ast) scopes.

	self assert: (ast scope lookupVar: 'last') isRemote.
	self assert: (ast scope lookupVar: 'val') isRemote.
	self assert: (ast scope lookupVar: 'val') vectorName equals: '0vector0'.
	self deny: (scopes second lookupVar: 'i') isRemote
]

{ #category : #'tests - blocks' }
OCASTClosureAnalyzerTest >> testExampleBlockArgument [
	| ast blockScope blockScope2 |
	ast := (OCOpalExamples >> #exampleBlockArgument) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 3.
	self assert: ast scope tempVector size equals: 0.
	self assert: ast scope copiedVars size equals: 0.

	self deny: (ast scope lookupVar: 'block') isRemote.
	self deny: (ast scope lookupVar: 'block1') isRemote.
	self deny: (ast scope lookupVar: 'block2') isRemote.

	blockScope := (OCScopesCollector new visitNode: ast) scopes second.

	self assert: blockScope tempVars size equals: 2.
	self assert: blockScope tempVector size equals: 0.
	self assert: blockScope copiedVars size equals: 1.
	self deny: (blockScope lookupVar: 'temp') isRemote.
	self assert: (blockScope lookupVar: 'temp') isEscapingRead.
	self assert: (blockScope lookupVar: 'temp') isWrite.
	self deny: (blockScope lookupVar: 'temp') isEscapingWrite.
	self deny: (blockScope lookupVar: 'arg') isRemote.


	blockScope2 := (OCScopesCollector new visitNode: ast) scopes third.
	self assert: blockScope2 tempVars size equals: 0.
	self assert: blockScope2 tempVector size equals: 0.
	self assert: blockScope2 copiedVars size equals: 1
]

{ #category : #'tests - optimized blocks' }
OCASTClosureAnalyzerTest >> testExampleSimpleBlockLocalIf [
	| ast  assignment var |
	ast := (OCOpalExamples>>#exampleSimpleBlockLocalIf) parseTree.
	ast doSemanticAnalysis.

	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 	
	var := assignment variable binding.
	self assert: var isWrite.
	self deny: var isEscaping.
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testExampleSimpleBlockLocalWhile [
	| ast scopes |
	ast := (OCOpalExamples >> #exampleSimpleBlockLocalWhile) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 1.
	self assert: (ast scope lookupVar: 'a') isRemote.

	scopes := (OCScopesCollector new visitNode: ast) scopes.

	self assert: scopes second tempVars size equals: 1.
	self assert: scopes second tempVector size equals: 0.
	self deny: (scopes second lookupVar: 'b') isRemote.
	self assert: (scopes second lookupVar: 'b') isArg.
	self deny: (scopes fourth lookupVar: 'hallo') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testExampleSimpleBlockNested [
	| ast scopes |
	ast := (OCOpalExamples >> #exampleSimpleBlockNested) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 2.
	self assert: ast scope tempVector size equals: 1.
	self deny: (ast scope lookupVar: 'a') isRemote.
	self deny: (ast scope lookupVar: 'dict') isRemote.
	self assert: (ast scope lookupVar: 'match') isRemote.

	scopes := (OCScopesCollector new visitNode: ast) scopes.

	self assert: scopes second tempVars size equals: 2.
	self assert: scopes second tempVector size equals: 0.
	self deny: (scopes second lookupVar: 'each') isRemote.
	self deny: (scopes second lookupVar: 'index') isRemote.

	self assert: scopes second copiedVars size equals: 3
]

{ #category : #'tests - blocks' }
OCASTClosureAnalyzerTest >> testExampleWhileModificationBefore [
	| ast |
	ast := (OCOpalExamples >> #exampleWhileModificationBefore) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 1.
	self assert: ast scope copiedVars size equals: 1.

	self assert: (ast scope lookupVar: 'index') isRemote
]

{ #category : #'tests - blocks' }
OCASTClosureAnalyzerTest >> testExampleWhileNoModification [
	"A block in an optimized loop with no modification on temp vars in the loop
only needs to copy the tempvars. No write -> no indirection vector."

	| ast |
	ast := (OCOpalExamples >> #exampleWhileNoModification) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 3.
	self assert: ast scope tempVector size equals: 0.
	self assert: ast scope copiedVars size equals: 3.
	self deny: (ast scope lookupVar: 'a') isRemote.
	self deny: (ast scope lookupVar: 'b') isRemote.
	self deny: (ast scope lookupVar: 'c') isRemote
]

{ #category : #'testing - variables' }
OCASTClosureAnalyzerTest >> testInlinedBlockArgumentIsArgumentVariable [
	| ast blockNode |
	ast := (OCOpalExamples>>#exampleForInlinedBlockArgument) parseTree.
	ast doSemanticAnalysis.
	blockNode := ast body statements first arguments first.
	self assert: blockNode arguments notEmpty.
	self assert: blockNode arguments first isArgument
	
]

{ #category : #'testing - variables' }
OCASTClosureAnalyzerTest >> testMethodArgumentIsArgumentVariable [
	| ast |
	ast := (OCOpalExamples>>#exampleWithArgument:) parseTree.
	ast doSemanticAnalysis .
	self assert: ast arguments notEmpty.
	self assert: ast arguments first isArgument
	
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testNestedBlocksRemoteInBlockCase1 [
	| ast scopes |
	ast := (OCOpalExamples >> #nestedBlocksRemoteInBlockCase1) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 0.
	self deny: (ast scope lookupVar: 'block') isRemote.

	scopes := (OCScopesCollector new visitNode: ast) scopes.

	self assert: scopes second tempVars size equals: 0.
	self assert: scopes second tempVector size equals: 2.
	self assert: (scopes second tempVector at: 'a') isRemote.
	self assert: (scopes second tempVector at: 'b') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testNestedBlocksRemoteInBlockCase2 [
	| ast scopes |
	ast := (OCOpalExamples >> #nestedBlocksRemoteInBlockCase2) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 0.
	self deny: (ast scope lookupVar: 'block') isRemote.

	scopes := (OCScopesCollector new visitNode: ast) scopes.

	self assert: scopes second tempVars size equals: 0.
	self assert: scopes second tempVector size equals: 0.

	self assert: scopes third tempVars size equals: 0.
	self assert: scopes third tempVector size equals: 1.
	self assert: (scopes third tempVector at: 'a') isRemote.

	self assert: scopes fourth tempVars size equals: 0.
	self assert: scopes fourth tempVector size equals: 0.

	self assert: scopes fifth tempVars size equals: 0.
	self assert: scopes fifth tempVector size equals: 1.
	self assert: (scopes fifth tempVector at: 'b') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testNestedBlocksRemoteInBlockCase3 [
	| ast scopes |
	ast := (OCOpalExamples >> #nestedBlocksRemoteInBlockCase3) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 1.
	self assert: (ast scope lookupVar: 'block') isRemote.

	scopes := (OCScopesCollector new visitNode: ast) scopes.

	self assert: scopes second tempVars size equals: 0.
	self assert: scopes second tempVector size equals: 0.

	self assert: scopes third tempVars size equals: 0.
	self assert: scopes third tempVector size equals: 1.
	self assert: (scopes third tempVector at: 'a') isRemote.

	self assert: scopes fourth tempVars size equals: 0.
	self assert: scopes fourth tempVector size equals: 0.

	self assert: scopes fifth tempVars size equals: 0.
	self assert: scopes fifth tempVector size equals: 1.
	self assert: (scopes fifth tempVector at: 'b') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testNoRemoteBlockArgument [
	| ast |
	ast := (OCOpalExamples >> #noRemoteBlockArgument) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 3.

	self deny: (ast scope lookupVar: 'block') isRemote.
	self deny: (ast scope lookupVar: 'block1') isRemote.
	self deny: (ast scope lookupVar: 'block2') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testNoRemoteReadInBlock [
	| ast |
	ast := (OCOpalExamples >> #noRemoteReadInBlock) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 0.

	self deny: (ast scope lookupVar: 'a') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testNoRemoteReadNestedBlocks [
	| ast scopes |
	ast := (OCOpalExamples >> #noRemoteReadNestedBlocks) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 0.

	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: scopes second tempVars size equals: 1.
	self assert: scopes second tempVector size equals: 0.
	self deny: (scopes second lookupVar: 'a') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testOptimizedBlockReadInBlock [
	| ast |
	ast := (OCOpalExamples >> #optimizedBlockReadInBlock) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 0.
	self deny: (ast scope lookupVar: 't1') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testOptimizedBlockWriteInBlock [
	| ast |
	ast := (OCOpalExamples >> #optimizedBlockWriteInBlock) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 0.
	self deny: (ast scope lookupVar: 't1') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testOptimizedBlockWriteInNestedBlock [
	| ast |
	ast := (OCOpalExamples >> #optimizedBlockWriteInNestedBlock) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 1.
	self assert: (ast scope lookupVar: 't1') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testOptimizedBlockWriteInNestedBlockCase2 [
	| ast |
	ast := (OCOpalExamples >> #optimizedBlockWriteInNestedBlockCase2) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 0.
	self deny: (ast scope lookupVar: 't1') isRemote.
	self assert: ast scope copiedVars size equals: 0
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testOptimizedBlockWriteInNestedBlockCase3 [
	| ast scopes |
	ast := (OCOpalExamples >> #optimizedBlockWriteInNestedBlockCase3) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 1.
	self assert: (ast scope lookupVar: 't1') isRemote.
	self assert: ast scope copiedVars size equals: 1.	"Is this correct, I think that the copied vars should be empty."

	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: scopes second tempVars size equals: 0.
	self assert: scopes second tempVector size equals: 0.
	self assert: scopes second copiedVars size equals: 1.
	scopes second copiedVars at: '0vector0' ifAbsent: [ self fail ].
	self assert: (scopes second copiedVars at: '0vector0') isStoringTempVector
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testOptimizedBlockWriteInNestedBlockCase4 [
	| ast scopes |
	ast := (OCOpalExamples >> #optimizedBlockWriteInNestedBlockCase4) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 1.
	self assert: (ast scope lookupVar: 't1') isRemote.
	self assert: ast scope copiedVars size equals: 1.	"Is this correct, I think that the copied vars should be empty."

	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: scopes second tempVars size equals: 0.
	self assert: scopes second tempVector size equals: 0.
	self assert: scopes second copiedVars size equals: 1.
	scopes second copiedVars at: '0vector0' ifAbsent: [ self fail ]
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testOptimizedBlockWrittenAfterClosedOverCase1 [
	| ast scopes |
	ast := (OCOpalExamples >> #optimizedBlockWrittenAfterClosedOverCase1) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 0.

	scopes := (OCScopesCollector new visitNode: ast) scopes.

	self assert: scopes third tempVars size equals: 0.
	self assert: scopes third tempVector size equals: 1.

	self deny: (scopes third lookupVar: 'index') isRemote.
	"problem: as block is optimized, this var does not need to be remote"
	self assert: (scopes third tempVector at: 'temp') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testOptimizedBlockWrittenAfterClosedOverCase2 [
	| ast scopes |
	ast := (OCOpalExamples >> #optimizedBlockWrittenAfterClosedOverCase2) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.

	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 0.
	self deny: (ast scope lookupVar: 'index') isRemote.

	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: (scopes third tempVector at: 'temp') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testSingleRemoteDifferentBlocksSameArgumentName [
	| ast |
	ast := (OCOpalExamples >> #singleRemoteDifferentBlocksSameArgumentName) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 2.
	self assert: ast scope tempVector size equals: 1.

	self deny: (ast scope lookupVar: 'b') isRemote.
	self deny: (ast scope lookupVar: 'c') isRemote.
	self assert: (ast scope lookupVar: 'z') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testSingleRemoteMethodArgument [
	| ast |
	ast := (OCOpalExamples >> #singleRemoteMethodArgument) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 1.

	self deny: (ast scope lookupVar: 'block') isRemote.
	self assert: (ast scope lookupVar: 'temp') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testSingleRemoteReadNestedBlocks [
	| ast |
	ast := (OCOpalExamples >> #singleRemoteReadNestedBlocks) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 1.

	self assert: (ast scope lookupVar: 'a') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testSingleRemoteTempVar [
	| ast |
	ast := (OCOpalExamples >> #singleRemoteTempVar) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 2.
	self assert: ast scope tempVector size equals: 1.

	self assert: (ast scope lookupVar: 'index') isRemote.
	self assert: (ast scope lookupVar: 'index') definingScope equals: ast scope.
	self deny: (ast scope lookupVar: 'block') isRemote.
	self deny: (ast scope lookupVar: 'collection') isRemote
]

{ #category : #'testing - variables' }
OCASTClosureAnalyzerTest >> testVariableDefintionScopeUpdate [
	"the scope of local var definitons needs to be updated, too"

	| ast |
	ast := OpalCompiler new parse:
			'm04_two_temps_one_temp_in_remote_vector

| t1 t2 |

t1 := 1.
[ t2 := t1 +  1.0 ] value.

^t2'.
	ast doSemanticAnalysis.
	self assert: ast temporaries second binding class equals: OCVectorTempVariable
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testWrittenAfterClosedOver [
	| ast |
	ast := (OCOpalExamples >> #writtenAfterClosedOver) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 0.
	self assert: ast scope tempVector size equals: 1.

	self assert: (ast scope lookupVar: 'a') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testsingleRemoteTempVarWhileWithTempNotInlined [
	| ast |
	ast := (OCOpalExamples >> #exampleWhileWithTempNotInlined) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 1.

	self assert: (ast scope lookupVar: 'index') isRemote.
	self assert: (ast scope lookupVar: 'index') definingScope equals: ast scope.
	self deny: (ast scope lookupVar: 'block') isRemote
]

{ #category : #'tests - special cases' }
OCASTClosureAnalyzerTest >> testsingleRemoteTempVarWrittenAfterClosedOver [
	| ast |
	ast := (OCOpalExamples >> #singleRemoteTempVarWrittenAfterClosedOver) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size equals: 1.
	self assert: ast scope tempVector size equals: 1.

	self assert: (ast scope lookupVar: 'index') isRemote.
	self assert: (ast scope lookupVar: 'index') definingScope equals: ast scope.
	self deny: (ast scope lookupVar: 'block') isRemote
]
