"
By default I do not nothing. I'm the NullObject of the fetcher hierarchy.
"
Class {
	#name : #CoEmptyFetcher,
	#superclass : #CoFetcher,
	#category : #'HeuristicCompletion-Core'
}

{ #category : #accessing }
CoEmptyFetcher >> entriesDo: aBlock [

	"Nothing"
]

{ #category : #accessing }
CoEmptyFetcher >> isEmptyCompletionFetcher [
	
	^ true
]
