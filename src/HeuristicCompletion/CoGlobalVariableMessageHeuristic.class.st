"
I implement a heuristic for messages sent to globals.
If the global object is available in the environment, 
 - if it is a class complete messages in its class side
 - if it is a global object complete messages in the global object's class

See my superclass for details.
"
Class {
	#name : #CoGlobalVariableMessageHeuristic,
	#superclass : #CoVariableMessageHeuristic,
	#category : #'HeuristicCompletion-Heuristics'
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
