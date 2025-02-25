"
I implement an heuristic for self message sends.
I provide a fetcher for messages for the hierarchy of self class.

See my superclass for details.
"
Class {
	#name : 'CoSelfMessageHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoSelfMessageHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aNode receiver isSelfVariable
]

{ #category : 'requests' }
CoSelfMessageHeuristic >> buildFetcherFor: aNode inContext: completionContext [
	| completionClass |

	"When in the playground, the completion class is not available.
	But self is bound to nil"
	completionClass := completionContext completionClass ifNil: [ nil class ].
	^ self newMessageInHierarchyFetcherForClass: completionClass inASTNode: aNode
]
