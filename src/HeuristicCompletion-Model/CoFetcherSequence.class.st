"
I'm a fetcher composed of a sequence of subfetchers. 
When executed, I execute my subfetchers one by one.

Sequences are created by sending the message #, between two fetchers.
"
Class {
	#name : 'CoFetcherSequence',
	#superclass : 'CoFetcher',
	#instVars : [
		'subcompletions'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'adding' }
CoFetcherSequence >> add: aCompletion [

	subcompletions add: aCompletion
]

{ #category : 'resetting' }
CoFetcherSequence >> context: anObject [

	super context: anObject.
	subcompletions do: [ :each | each context: anObject ]
]

{ #category : 'enumerating' }
CoFetcherSequence >> entriesDo: aBlock [
	subcompletions do: [ :completion | completion entriesDo: aBlock ]
]

{ #category : 'enumerating' }
CoFetcherSequence >> filter: aFilter [

	super filter: aFilter.
	subcompletions do: [ :completion | completion filter: aFilter ]
]

{ #category : 'initialization' }
CoFetcherSequence >> initialize [

	super initialize.
	subcompletions := OrderedCollection new
]

{ #category : 'resetting' }
CoFetcherSequence >> reset [

	super reset.
	subcompletions do: [ :each | each reset ]
]
