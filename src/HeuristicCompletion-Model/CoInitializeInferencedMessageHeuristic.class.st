"
I am an heuristic using depth-bound type inference to type variables.
I autocomplete messages in the hierarchy of the found types.
"
Class {
	#name : 'CoInitializeInferencedMessageHeuristic',
	#superclass : 'CoASTNodeFetcherHeuristic',
	#category : 'HeuristicCompletion-Model-InitializeInferenceHeuristic',
	#package : 'HeuristicCompletion-Model',
	#tag : 'InitializeInferenceHeuristic'
}

{ #category : 'requests' }
CoInitializeInferencedMessageHeuristic >> appliesForNode: aNode inContext: aContext [

	^ aContext completionClass isNotNil
		and: [ aNode receiver isVariable ]
]

{ #category : 'requests' }
CoInitializeInferencedMessageHeuristic >> buildFetcherFor: aNode inContext: aContext [

	| variables fetcher |
	variables := self inferTypesIn: aContext completionClass.
	fetcher := CoEmptyFetcher new.
	variables
		at: aNode receiver name
		ifPresent: [ :types |
			types do: [ :each |
				fetcher := (self newMessageInHierarchyFetcherForClass: each inASTNode: aNode), fetcher ] ].
	^ fetcher
]

{ #category : 'requests' }
CoInitializeInferencedMessageHeuristic >> inferTypesIn: aClass [

	^ CoTypeInferencer new
		inferFrom: aClass;
		variables
]
