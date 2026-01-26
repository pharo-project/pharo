"
I implement an heuristic for messages sent to globals.
If the global object is available in the environment, 
 - if it is a class complete messages in it class side
 - if it is a global object complete messages in the global object's class

See my superclass for details.
"
Class {
	#name : 'CoGlobalVariableMessageHeuristic',
	#superclass : 'CoVariableMessageHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoGlobalVariableMessageHeuristic >> buildFetcherFor: aMessageNode inContext: completionContext [

	completionContext
		environmentAt: aMessageNode receiver name
		ifPresent: [ :global | ^ self newFetcherForGlobal: global inASTNode: aMessageNode ].

	^ CoEmptyFetcher new
]

{ #category : 'requests' }
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
