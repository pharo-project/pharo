"
I implement an heuristic for messages sent to variables in the form ""aX"" or ""anX"".
If a class X is available in the environment, I return a fetcher for X's instance side messages.

See my superclass for details.
"
Class {
	#name : 'CoVariableWithTypeNameMessageHeuristic',
	#superclass : 'CoVariableMessageHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoVariableWithTypeNameMessageHeuristic >> buildFetcherFor: aMessageNode inContext: completionContext [

	"Heuristic: variables with type information.
	This is maybe an instance of this class, autocomplete for that class first, then continue with normal completion.
	E.g., aCollection => use completion for Collection class first, then normal collection.
	There are two cases: aPoint and anASTCache"
	aMessageNode receiver name size > 1
		and: [ completionContext
				environmentAt: aMessageNode receiver name allButFirst asSymbol
				ifPresent: [ :global |
					^ self newFetcherForMaybeGlobalInstance: global inASTNode: aMessageNode ] ].

	aMessageNode receiver name size > 2
		and: [ completionContext
				environmentAt: (aMessageNode receiver name allButFirst: 2) asSymbol
				ifPresent: [ :global |
					^ self newFetcherForMaybeGlobalInstance: global inASTNode: aMessageNode ] ].

	^ CoEmptyFetcher new
]

{ #category : 'requests' }
CoVariableWithTypeNameMessageHeuristic >> newFetcherForMaybeGlobalInstance: aGlobal inASTNode: astNode [
	"Fetcher for messages to instance variables containing type information.
	For example: aCollection could autocomplete collection messages."

	"This is a class or trait. Autocomplete instance-side messages"
	aGlobal isClassOrTrait ifTrue: [
		^ CoClassImplementedMessagesFetcher new
			astNode: astNode;
			completionClass: aGlobal instanceSide;
			forHierarchy ].

	"This is not a class binding but a global object.
	E.g., aSmalltalk
	Autocomplete based on its class"
	^ CoClassImplementedMessagesFetcher new
		astNode: astNode;
		completionClass: aGlobal class;
		forHierarchy
]
