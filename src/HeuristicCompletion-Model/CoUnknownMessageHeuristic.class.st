"
I implement a default heuristic for when nothing else can be done.
I provide a fetcher for messages for the full system.
I am usually at the end of the chain of responsibility.

See my superclass for details.
"
Class {
	#name : 'CoUnknownMessageHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoUnknownMessageHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isMethod or: [ aNode isMessage ]
]

{ #category : 'requests' }
CoUnknownMessageHeuristic >> buildFetcherFor: aNode inContext: completionContext [
	"This is the main heuristic used for those messages where we cannot guess the type.
	I can be used also after more fine heuristics, to complete guessed heuristics that cannot be trusted too much, like variable names"

	"If the context allows it, autocomplete first messages in the current package, then the rest in the system"

	^ CoGlobalSelectorFetcher new
			astNode: aNode;
			yourself
]
