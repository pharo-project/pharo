"
By default, I do nothing. I'm the NullObject of the fetcher hierarchy.
"
Class {
	#name : #CoEmptyFetcher,
	#superclass : #CoFetcher,
	#category : #'HeuristicCompletion-Core'
}

{ #category : #enumerating }
CoEmptyFetcher >> entriesDo: aBlock [

	"Nothing"
]

{ #category : #testing }
CoEmptyFetcher >> isEmptyCompletionFetcher [
	
	^ true
]
