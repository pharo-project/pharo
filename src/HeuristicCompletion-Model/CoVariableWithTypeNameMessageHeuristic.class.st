Class {
	#name : #CoVariableWithTypeNameMessageHeuristic,
	#superclass : #CoVariableMessageHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoVariableWithTypeNameMessageHeuristic >> buildFetcherFor: aRBMessageNode inContext: completionContext [

	"Heuristic: variables with type information.
	This is maybe an instance of this class, autocomplete for that class first, then continue with normal completion.
	E.g., aCollection => use completion for Collection class first, then normal collection.
	There are two cases: aPoint and anASTCache"
	aRBMessageNode receiver name size > 1
		and: [ completionContext
				environmentAt: aRBMessageNode receiver name allButFirst asSymbol
				ifPresent: [ :global | 
					^ self newFetcherForMaybeGlobalInstance: global ] ].
	
	aRBMessageNode receiver name size > 2
		and: [ completionContext
				environmentAt: (aRBMessageNode receiver name allButFirst: 2) asSymbol
				ifPresent: [ :global | 
					^ self newFetcherForMaybeGlobalInstance: global ] ].
				
	^ CoEmptyFetcher new
]

{ #category : #requests }
CoVariableWithTypeNameMessageHeuristic >> newFetcherForMaybeGlobalInstance: aGlobal [
	"Fetcher for messages to instance variables containing type information.
	For example: aCollection could autocomplete collection messages."

	"This is a class or trait. Autocomplete instance-side messages" 
	aGlobal isClassOrTrait ifTrue: [
		^ CoClassImplementedMessagesFetcher new
			completionClass: aGlobal instanceSide;
			forHierarchy ].

	"This is not a class binding but a global object.
	E.g., aSmalltalk
	Autocomplete based on its class"
	^ CoClassImplementedMessagesFetcher new
		completionClass: aGlobal class;
		forHierarchy
]
