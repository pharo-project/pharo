"
I implement an heuristic for messages sent to literals.
If there is a literal as receiver, create a fetcher for messages of its class.

See my superclass for details.
"
Class {
	#name : 'CoLiteralMessageHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoLiteralMessageHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode receiver isLiteralNode
]

{ #category : 'requests' }
CoLiteralMessageHeuristic >> buildFetcherFor: aMessageNode inContext: aContext [

	^ self newMessageInHierarchyFetcherForClass: aMessageNode receiver value class inASTNode: aMessageNode
]
