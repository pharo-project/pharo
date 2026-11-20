"
I ask the type inferencer for the type of that receiver expression, then propose selectors from the inferred type hierarchy.

"
Class {
	#name : 'CoInferredReceiverMessageHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoInferredReceiverMessageHeuristic >> appliesForNode: aMessageNode inContext: aContext [

	^ aMessageNode isMessage
		and: [ aMessageNode receiver isMessage ]
]

{ #category : 'requests' }
CoInferredReceiverMessageHeuristic >> buildFetcherFor: aMessageNode inContext: aContext [

	| receiverTypes fetcher |

	receiverTypes := self
		inferredTypesOf: aMessageNode receiver
		inContext: aContext.

	receiverTypes := receiverTypes select: [ :each |
		self canCompleteMessagesFor: each ].

	receiverTypes ifEmpty: [ ^ CoEmptyFetcher new ].

	fetcher := CoEmptyFetcher new.

	receiverTypes do: [ :each |
		fetcher := (self
			newMessageInHierarchyFetcherForClass: each
			inASTNode: aMessageNode), fetcher ].

	^ fetcher
]

{ #category : 'requests' }
CoInferredReceiverMessageHeuristic >> canCompleteMessagesFor: aType [

	^ aType notNil
		and: [
			(aType isKindOf: CoUnknownType) not
				and: [ aType respondsTo: #selectorsDo ] ]
]

{ #category : 'requests' }
CoInferredReceiverMessageHeuristic >> inferredTypesOf: aReceiverNode inContext: aContext [

	| inferer |

	inferer := CoTypeInferencer new.
	inferer receiverClass: (aContext completionClass ifNil: [ Object ]).

	^ [
		(aReceiverNode acceptVisitor: inferer) asSet asOrderedCollection
	] on: Error do: [ :error |
		"Completion must never break the editor. If inference fails,
		let the next heuristic, usually CoUnknownMessageHeuristic, answer."
		#() ]
]
