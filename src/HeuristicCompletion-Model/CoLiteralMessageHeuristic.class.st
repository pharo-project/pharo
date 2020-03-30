Class {
	#name : #CoLiteralMessageHeuristic,
	#superclass : #CoASTNodeFetcherHeuristic,
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoLiteralMessageHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode receiver isLiteralNode
]

{ #category : #requests }
CoLiteralMessageHeuristic >> buildFetcherFor: aRBMessageNode inContext: aContext [

	^ self newMessageInHierarchyFetcherForClass: aRBMessageNode receiver value class inASTNode: aRBMessageNode
]
