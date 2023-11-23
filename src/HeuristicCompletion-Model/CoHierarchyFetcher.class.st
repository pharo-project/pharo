"
I am a complishon fetcher decorator for ClassBasedComplishonFetcher that will iterate from my child up to the end of the class hierarchy.
I'll yield all results found in one, then move to the superclass, and so on.
"
Class {
	#name : 'CoHierarchyFetcher',
	#superclass : 'CoFetcherDecorator',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoHierarchyFetcher >> entriesDo: aBlock [
	| currentCompletionFetcher |
	currentCompletionFetcher := decoree.
	[ currentCompletionFetcher isEmptyCompletionFetcher ]
		whileFalse: [ currentCompletionFetcher entriesDo: aBlock.
			currentCompletionFetcher := currentCompletionFetcher onSuperclass ]
]
