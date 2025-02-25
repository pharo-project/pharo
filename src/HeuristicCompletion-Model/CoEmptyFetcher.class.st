"
By default I do not nothing. I'm the NullObject of the fetcher hierarchy.
"
Class {
	#name : 'CoEmptyFetcher',
	#superclass : 'CoFetcher',
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'enumerating' }
CoEmptyFetcher >> entriesDo: aBlock [

	"Nothing"
]

{ #category : 'testing' }
CoEmptyFetcher >> isEmptyCompletionFetcher [

	^ true
]
