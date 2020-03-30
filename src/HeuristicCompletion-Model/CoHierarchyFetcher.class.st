Class {
	#name : #CoHierarchyFetcher,
	#superclass : #CoFetcherDecorator,
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #enumerating }
CoHierarchyFetcher >> entriesDo: aBlock [
	| currentCompletionFetcher |
	currentCompletionFetcher := decoree.
	[ currentCompletionFetcher isEmptyCompletionFetcher ]
		whileFalse: [ currentCompletionFetcher entriesDo: aBlock.
			currentCompletionFetcher := currentCompletionFetcher onSuperclass ]
]
