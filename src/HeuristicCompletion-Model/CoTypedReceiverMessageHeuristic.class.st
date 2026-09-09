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
CoTypedReceiverMessageHeuristic >> buildFetcherFor: aMessageNode inContext: aContext [

	^ self
		  newMessageInHierarchyFetcherForClass:
		  (aMessageNode receiver propertyAt: #type)
		  inASTNode: aMessageNode
]
