"
I am an item in the history when using the CoNarrowHistoryFetcher.
I represent the intermediate results of a query, allowing to narrowing and unnarrowing without needing to reset the decoree fetcher.
"
Class {
	#name : 'CoNarrowHistoryItem',
	#superclass : 'Object',
	#instVars : [
		'fetcher',
		'results',
		'hasFinished'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'instance creation' }
CoNarrowHistoryItem class >> forFetcher: fetcher results: results hasFinished: hasFinished [

	^ self new
		fetcher: fetcher;
		results: results;
		hasFinished: hasFinished;
		yourself
]

{ #category : 'accessing' }
CoNarrowHistoryItem >> fetcher [

	^ fetcher
]

{ #category : 'accessing' }
CoNarrowHistoryItem >> fetcher: anObject [

	fetcher := anObject
]

{ #category : 'accessing' }
CoNarrowHistoryItem >> hasFinished [

	^ hasFinished
]

{ #category : 'accessing' }
CoNarrowHistoryItem >> hasFinished: anObject [

	hasFinished := anObject
]

{ #category : 'accessing' }
CoNarrowHistoryItem >> results [

	^ results
]

{ #category : 'accessing' }
CoNarrowHistoryItem >> results: anObject [

	results := anObject
]
