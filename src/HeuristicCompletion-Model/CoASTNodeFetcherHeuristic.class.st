Class {
	#name : #CoASTNodeFetcherHeuristic,
	#superclass : #Object,
	#instVars : [
		'next'
	],
	#category : #'HeuristicCompletion-Model-Heuristics'
}

{ #category : #requests }
CoASTNodeFetcherHeuristic >> appliesForNode: aRBMessageNode inContext: aContext [
	
	^ false
]

{ #category : #requests }
CoASTNodeFetcherHeuristic >> buildFetcherFor: aRBMessageNode inContext: aContext [

	^ CoEmptyFetcher new
]

{ #category : #requests }
CoASTNodeFetcherHeuristic >> fetcherFor: aRBMessageNode inContext: aContext [

	| nextFetcher |
	nextFetcher := self next fetcherFor: aRBMessageNode inContext: aContext.
	^ (self appliesForNode: aRBMessageNode inContext: aContext)
		ifTrue: [
			(self buildFetcherFor: aRBMessageNode inContext: aContext),
				nextFetcher ]
		ifFalse: [ nextFetcher ]
]

{ #category : #initialization }
CoASTNodeFetcherHeuristic >> initialize [
	super initialize.
	next := CoNullFetcherHeuristic new
]

{ #category : #testing }
CoASTNodeFetcherHeuristic >> isStatisticsHeuristicWrapper [
	
	^ false
]

{ #category : #'fetcher-creation' }
CoASTNodeFetcherHeuristic >> newMessageInHierarchyFetcherForClass: aClass [
	
	^ CoClassImplementedMessagesFetcher new
		completionClass: aClass;
		forHierarchy
]

{ #category : #accessing }
CoASTNodeFetcherHeuristic >> next [
	^ next
]

{ #category : #accessing }
CoASTNodeFetcherHeuristic >> next: nextHeuristic [ 
	next := nextHeuristic
]
