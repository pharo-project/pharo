Class {
	#name : #CoResultSet,
	#superclass : #Object,
	#instVars : [
		'results',
		'fetcher',
		'completionString'
	],
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #'instance creation' }
CoResultSet class >> fetcher: aFetcher [
	
	^ self new
		fetcher: aFetcher;
		yourself
]

{ #category : #'instance creation' }
CoResultSet class >> onContext: aCompletionContext fetcher: aFetcher [
	
	^ self new
		fetcher: aFetcher;
		yourself
]

{ #category : #accessing }
CoResultSet >> completionToken [
	
	^ completionString
]

{ #category : #enumerating }
CoResultSet >> ensureResults: aSize [
	
	results size >= aSize ifTrue: [ ^ self ].
	self fetch: aSize - results size.
]

{ #category : #fetching }
CoResultSet >> fetch: anInteger [ 
	
	| newResults |
	newResults := fetcher next: anInteger.
	results addAll: newResults
]

{ #category : #fetching }
CoResultSet >> fetchAll [
	
	results addAll: fetcher upToEnd
]

{ #category : #accessing }
CoResultSet >> fetcher [
	^ fetcher
]

{ #category : #accessing }
CoResultSet >> fetcher: anObject [

	fetcher := anObject
]

{ #category : #'api-filtering' }
CoResultSet >> filterWithString: aString [ 
	
	"Narrow results"
	aString size >= completionString size
		ifTrue: [ 
			"Filter existing results".
			fetcher := fetcher narrowFilterBlock: [ :e | aString isEmpty or: [ e contents beginsWith: aString ] ].
			results := results select: [ :e | aString isEmpty or: [ e contents beginsWith: aString ] ] ]
		ifFalse: [
			"Smaller filter, reset results and filter"
			results := OrderedCollection new.
			fetcher reset.
			fetcher := fetcher select: [ :e | aString isEmpty or: [ e contents beginsWith: aString ] ].
		].
	completionString := aString
]

{ #category : #enumerating }
CoResultSet >> first [
	
	self ensureResults: 1.
	^ results first
]

{ #category : #accessing }
CoResultSet >> first: anInteger [ 

	self ensureResults: anInteger.	
	
	results size > anInteger
		ifTrue: [ ^ results first: anInteger ].
	^ results
]

{ #category : #testing }
CoResultSet >> hasMoreElements [
	
	^ fetcher atEnd not
]

{ #category : #initialization }
CoResultSet >> initialize [

	super initialize.
	results := OrderedCollection new.
	completionString := ''.
]

{ #category : #testing }
CoResultSet >> notEmpty [
	
	^ results notEmpty or: [ fetcher atEnd not ]
]

{ #category : #'api-filtering' }
CoResultSet >> resetFilter [
	
	results removeAll.
	fetcher := fetcher withoutFilter.
]

{ #category : #accessing }
CoResultSet >> results [
	^ results
]

{ #category : #accessing }
CoResultSet >> results: anObject [
	results := anObject
]
