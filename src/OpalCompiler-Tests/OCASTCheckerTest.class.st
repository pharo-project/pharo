Class {
	#name : #OCASTCheckerTest,
	#superclass : #TestCase,
	#category : 'OpalCompiler-Tests-AST'
}

{ #category : #tools }
OCASTCheckerTest >> nameAnalysisNoClosureIn: class for: ast [
	"Look up vars in classOrScope.  My tree will be annotated with bindings to Scopes and Variables."

	OCASTSemanticAnalyzer new
		compilationContext: class compiler compilationContext;
		visitNode: ast.
]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testDoubleRemoteAnidatedBlocks [
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#doubleRemoteAnidatedBlocks) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self assert:  (ast scope lookupVar: 'last') isEscaping.
	self assert: (ast scope lookupVar: 'val') isEscaping.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: scopes size = 4.
	
	self assert: (scopes second lookupVar: 'i') isEscaping. "This is due to the inlined block."

	
	self assert: scopes third tempVars size = 1.
	self deny: (scopes third lookupVar: 'continue') isEscaping. "It is not escaping since is being accessed in an optimized block."



]

{ #category : #'testing - simple' }
OCASTCheckerTest >> testExampleIfNotNilReturnNil [
	| ast |
	ast := (OCOpalExamples>>#exampleIfNotNilReturnNil) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
]

{ #category : #'testing - blocks - optimized' }
OCASTCheckerTest >> testExampleInlineBlockCollectionLR3 [
	| ast |
	ast := (OCOpalExamples>>#exampleInlineBlockCollectionLR3) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1. "index is a temp of the outer method due to optimized block"
]

{ #category : #'testing - primitives' }
OCASTCheckerTest >> testExamplePrimitiveErrorCode [
	| method ast ir newMethod |
	ast := (OCOpalExamples>>#examplePrimitiveErrorCode) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.

	self assert: (ast scope lookupVar: 'code') isTemp.

	
]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testExampleSelf [
	| ast assignment vars |
	ast := (OCOpalExamples>>#exampleSelf) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	self assert: (ast scope lookupVar: 'self') isSelf.
	
	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 
	self assert: assignment value binding isSelf.
]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testExampleSuper [
	| ast assignment vars |
	ast := (OCOpalExamples>>#exampleSuper) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	self assert: (ast scope lookupVar: 'super') isSuper.
	
	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 
	self assert: assignment value binding isSuper.
]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testExampleThisContext [ 
	| ast assignment vars |
	ast := (OCOpalExamples>>#exampleThisContext) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	self assert: (ast scope lookupVar: 'thisContext') isThisContext.
	
	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 
	self assert: assignment value binding isThisContext.
]

{ #category : #'testing - blocks - optimized' }
OCASTCheckerTest >> testExampleToDoArgument [
	| ast |
	ast := (OCOpalExamples>>#exampleToDoArgument) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testInstanceVar [
	| ast assignment vars |
	ast := (OCOpalExamples>>#exampleiVar) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars isEmpty.
	
	self assert: ast scope outerScope isInstanceScope.
	self assert: (ast scope outerScope lookupVar: 'iVar') isInstance.
	
	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 
	self assert: assignment variable isInstance.
]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testNoRemoteBlockArgument [
	| ast assignment vars |
	ast := (OCOpalExamples>>#noRemoteBlockArgument) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	
	self deny: (ast scope lookupVar: 'block') isEscaping .
	self deny: (ast scope lookupVar: 'block1') isEscaping.
	self deny: (ast scope lookupVar: 'block2') isEscaping.



]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testNoRemoteBlockReturn [
	| ast assignment vars |
	ast := (OCOpalExamples>>#noRemoteBlockReturn) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
	



]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testNoRemoteBlockTemp [
	| ast assignment vars |
	ast := (OCOpalExamples>>#noRemoteBlockTemp) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self deny: (ast scope lookupVar: 'block1') isEscaping.
	self deny: (ast scope lookupVar: 'block2') isEscaping.



]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testNoRemoteMethodTemp [
	| ast assignment vars |
	ast := (OCOpalExamples>>#noRemoteMethodTemp) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self deny: (ast scope lookupVar: 'block1') isEscaping.
	self deny: (ast scope lookupVar: 'block2') isEscaping.
	



]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testOptimizedBlockWrittenAfterClosedOverCase1 [
	| ast scopes |
	ast := (OCOpalExamples>>#optimizedBlockWrittenAfterClosedOverCase1) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	
	self deny: (ast scope lookupVar: 'index') isEscaping. 
	self assert: (ast scope lookupVar: 'index') definingScope  = ast scope.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self assert: (scopes third lookupVar: 'temp') isWrite.
	self assert: (scopes third lookupVar: 'temp') isEscaping.


]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testOptimizedBlockWrittenAfterClosedOverCase2 [
	| ast scopes |
	ast := (OCOpalExamples>>#optimizedBlockWrittenAfterClosedOverCase2) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	
	self deny: (ast scope lookupVar: 'index') isEscaping. 
	self assert: (ast scope lookupVar: 'index') definingScope= ast scope.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self assert: ( scopes third lookupVar: 'temp') isEscapingWrite.
	self assert: ( scopes third lookupVar: 'temp') isEscaping.


]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testOptimizedBlocksAndSameNameTemps [
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#optimizedBlocksAndSameNameTemps) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self deny: (ast scope lookupVar: 's') isRemote.
	self deny: (ast scope lookupVar: 'c') isRemote.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self deny: (scopes second lookupVar: 'a') isRemote.
	self deny: (scopes fourth lookupVar: 'i') isRemote.



]

{ #category : #'testing - simple' }
OCASTCheckerTest >> testReturn1 [
	| ast |
	ast := (OCOpalExamples>>#exampleReturn1) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars isEmpty.
]

{ #category : #'testing - simple' }
OCASTCheckerTest >> testSemanticAnalysisOnNonMethodNode [
	| ast |
	{[ 1 + 2 ].
	thisContext.
	(OCOpalExamples >> #exampleReturn1)}
		do: [ :object | 
			ast := object sourceNode.
			ast doSemanticAnalysis ].
	#('1' 'true' 'nil' '1 + 2' '^1' '1 + 2. 2 + 3' '#(1 true)' '{ #foo . 1 }' '1+2;+3')
		do: [ :source | 
			ast := RBParser parseExpression: source.
			ast doSemanticAnalysis ].
	ast := RBParser parseMethod: 'foo 1 + 2'.
	ast doSemanticAnalysis
]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testSingleRemoteDifferentBlocksSameArgumentName [
	| ast assignment vars |
	ast := (OCOpalExamples>>#singleRemoteDifferentBlocksSameArgumentName) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	
	self deny: (ast scope lookupVar: 'b') isEscaping.
	self deny: (ast scope lookupVar: 'c') isEscaping.
	self assert: (ast scope lookupVar: 'z') isEscaping.



]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testSingleRemoteMethodArgument [
	| ast assignment vars |
	ast := (OCOpalExamples>>#singleRemoteMethodArgument) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self assert: (ast scope lookupVar: 'temp') isEscaping.
	self assert: (ast scope lookupVar: 'temp') isWrite.



]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testSingleRemoteTempVar [
	| ast |
	ast := (OCOpalExamples>>#singleRemoteTempVar) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	
	self assert: (ast scope lookupVar: 'index') isEscaping.
	self assert: (ast scope lookupVar: 'index') definingScope = ast scope.
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self assert: (ast scope lookupVar: 'theCollection') isEscaping.
	self assert: (ast scope lookupVar: 'block') isTemp.
	self assert: (ast scope lookupVar: 'theCollection') isTemp.
	self deny: (ast scope lookupVar: 'theCollection') isInstance.
	self deny: (ast scope lookupVar: 'index') isInstance.
	self deny: (ast scope lookupVar: 'block') isInstance.


]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testsingleRemoteTempVarWhileWithTempNotInlined [
	| ast |
	ast := (OCOpalExamples>>#exampleWhileWithTempNotInlined) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self assert: (ast scope lookupVar: 'index') isEscaping.
	self assert: (ast scope lookupVar: 'index') definingScope = ast scope.
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self assert: (ast scope lookupVar: 'block') isTemp.



]

{ #category : #'testing - variables' }
OCASTCheckerTest >> testsingleRemoteTempVarWrittenAfterClosedOver [
	| ast |
	ast := (OCOpalExamples>>#singleRemoteTempVarWrittenAfterClosedOver) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self assert: (ast scope lookupVar: 'index') isWrite.
	self assert: (ast scope lookupVar: 'index') definingScope = ast scope.
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self assert: (ast scope lookupVar: 'block') isTemp.


]
