"
I am a fetcher looking for the messages implemented in a class.
"
Class {
	#name : 'CoClassImplementedMessagesFetcher',
	#superclass : 'CoClassBasedFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoClassImplementedMessagesFetcher >> entriesDo: aBlock [

	self completionClass selectorsDo: [ :e |
		aBlock value: (NECSelectorEntry contents: e node: astNode)]
]
