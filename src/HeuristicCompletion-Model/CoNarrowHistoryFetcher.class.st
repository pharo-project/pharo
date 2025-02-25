"
I am a decorator that allows the narrowing and unnarrowing without needing to reset the decoree.
I store the intermediate state of the query and perform the subsequent query taking care of the results already handled.
"
Class {
	#name : 'CoNarrowHistoryFetcher',
	#superclass : 'CoFetcherDecorator',
	#instVars : [
		'currentResults',
		'history',
		'currentKey',
		'currentFinished'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'enumerating' }
CoNarrowHistoryFetcher >> entriesDo: aBlock [

	currentResults do: aBlock.

	"If previously I have detected that the decoree has not found entries, we need to avoid getting them again"
	currentFinished ifTrue: [ ^ self ].

	decoree entriesDo: [ :elem |
		currentResults add: elem.
		aBlock value: elem ].

	currentFinished := true
]

{ #category : 'initialization' }
CoNarrowHistoryFetcher >> initialize [

	super initialize.
	currentResults := OrderedCollection new.
	currentKey := ''.
	currentFinished := false.
	history := Dictionary new
]

{ #category : 'composing' }
CoNarrowHistoryFetcher >> narrowFilter: aFilter narrowKey: aKey [

	history at: currentKey put: (CoNarrowHistoryItem forFetcher: decoree results: currentResults hasFinished: currentFinished).

	currentResults := currentResults select: [ :e | aFilter accepts: e ].

	decoree := decoree narrowFilter: aFilter narrowKey: currentKey.
	filter := aFilter.
	generator := nil.
	currentFinished := false.

	currentKey := aKey.

	^ self
]

{ #category : 'initialization' }
CoNarrowHistoryFetcher >> reset [

	super reset.
	currentResults := OrderedCollection new.
	currentFinished := false.
	history := Dictionary new
]

{ #category : 'composing' }
CoNarrowHistoryFetcher >> unnarrowFilter: aFilter narrowKey: aKey [

	history at: aKey
		ifPresent: [ :historyItem |
			decoree := historyItem fetcher.
			currentResults := historyItem results.
			currentFinished := historyItem hasFinished.
			filter := aFilter.
			generator := nil.
			 ]
		ifAbsent: [
			self filter: aFilter.
			decoree := decoree unnarrowFilter: aFilter narrowKey: aKey.
			currentFinished := false ].

	currentKey := aKey
]

{ #category : 'composing' }
CoNarrowHistoryFetcher >> withNarrowHistory [

	^ self
]
