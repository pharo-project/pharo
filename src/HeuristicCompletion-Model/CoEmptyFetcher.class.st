Class {
	#name : #CoEmptyFetcher,
	#superclass : #CoFetcher,
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #enumerating }
CoEmptyFetcher >> entriesDo: aBlock [

	"Nothing"
]

{ #category : #testing }
CoEmptyFetcher >> isEmptyCompletionFetcher [
	
	^ true
]
