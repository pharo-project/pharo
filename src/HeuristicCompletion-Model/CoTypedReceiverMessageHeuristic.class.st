Class {
	#name : 'CoTypedReceiverMessageHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoTypedReceiverMessageHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode receiver
		  propertyAt: #type
		  ifPresent: [ :t | t ~= UndefinedObject ]
		  ifAbsent: [ false ]
]

{ #category : 'requests' }
CoTypedReceiverMessageHeuristic >> buildFetcherFor: aRBMessageNode inContext: aContext [

	^ self
		  newMessageInHierarchyFetcherForClass:
		  (aRBMessageNode receiver propertyAt: #type)
		  inASTNode: aRBMessageNode
]
