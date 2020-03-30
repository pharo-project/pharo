Class {
	#name : #CoGlobalVariableMessageHeuristic,
	#superclass : #CoVariableMessageHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoGlobalVariableMessageHeuristic >> buildFetcherFor: aRBMessageNode inContext: completionContext [

	completionContext
		environmentAt: aRBMessageNode receiver name
		ifPresent: [ :global | ^ self newFetcherForGlobal: global ].

	^ CoEmptyFetcher new
]

{ #category : #requests }
CoGlobalVariableMessageHeuristic >> newFetcherForGlobal: aGlobal [

	"This is a class or trait. Autocomplete class-side messages" 
	aGlobal isClassOrTrait ifTrue: [
		^ CoClassImplementedMessagesFetcher new
			completionClass: aGlobal classSide;
			forHierarchy ].

	"This is not a class binding but a global object. Autocomplete based on its class"
	^ CoClassImplementedMessagesFetcher new
		completionClass: aGlobal class;
		forHierarchy
]
