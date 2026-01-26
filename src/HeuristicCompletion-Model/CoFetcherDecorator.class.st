"
I am an abstract decorator superclass encapsulating common tasks that should be done with my decoree.
I am a complishon fetcher exposing the same API, and redirecting it to my decoree by default.
My decoree is also a complishon fetcher.
"
Class {
	#name : 'CoFetcherDecorator',
	#superclass : 'CoFetcher',
	#instVars : [
		'decoree'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'testing' }
CoFetcherDecorator class >> isAbstract [

	^ self == CoFetcherDecorator
]

{ #category : 'private' }
CoFetcherDecorator >> acceptsEntry: anEntry [

	"I trust my decoree. If I have the entry its because it accepted the entry"
	^ true
]

{ #category : 'accessing' }
CoFetcherDecorator >> decoree: anotherFetcher [

	decoree := anotherFetcher
]

{ #category : 'private' }
CoFetcherDecorator >> entriesDo: aBlock [

	^ decoree entriesDo: aBlock 
]

{ #category : 'accessing' }
CoFetcherDecorator >> filter: aFilter [

	super filter: aFilter.
	decoree filter: aFilter
]

{ #category : 'resetting' }
CoFetcherDecorator >> reset [

	super reset.
	decoree reset
]
