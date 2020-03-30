Class {
	#name : #CoGlobalVariableMessageHeuristic,
	#superclass : #CoVariableMessageHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoGlobalVariableMessageHeuristic >> buildFetcherFor: aRBMessageNode inContext: completionContext [

	completionContext
		environmentAt: aRBMessageNode receiver name
		ifPresent: [ :global | ^ self newFetcherForGlobal: global inASTNode: aRBMessageNode ].

	^ CoEmptyFetcher new
]

{ #category : #requests }
CoGlobalVariableMessageHeuristic >> newFetcherForGlobal: aGlobal inASTNode: ast [

	"This is a class or trait. Autocomplete class-side messages" 
	aGlobal isClassOrTrait ifTrue: [
		^ CoClassImplementedMessagesFetcher new
			astNode: ast;
			completionClass: aGlobal classSide;
			forHierarchy ].

	"This is not a class binding but a global object. Autocomplete based on its class"
	^ CoClassImplementedMessagesFetcher new
		astNode: ast;
		completionClass: aGlobal class;
		forHierarchy
]
