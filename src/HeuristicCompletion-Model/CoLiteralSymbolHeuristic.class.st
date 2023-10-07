"
I provide a fetcher for literals symbols for the full system.

See my superclass for details.
"
Class {
	#name : 'CoLiteralSymbolHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoLiteralSymbolHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode isLiteralNode and: [ aNode value isSymbol ]
]

{ #category : 'requests' }
CoLiteralSymbolHeuristic >> buildFetcherFor: aNode inContext: completionContext [

	^ CoGlobalSymbolFetcher new
			astNode: aNode;
			yourself


]
