"
I am a null fetcher heuristic, returning an empty fetcher
"
Class {
	#name : 'CoNullFetcherHeuristic',
	#superclass : 'Object',
	#category : 'HeuristicCompletion-Model-Heuristics',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Heuristics'
}

{ #category : 'requests' }
CoNullFetcherHeuristic >> fetcherFor: aNode inContext: anObject [

	^ CoEmptyFetcher new
]

{ #category : 'testing' }
CoNullFetcherHeuristic >> isNullHeuristic [

	^ true
]
