"
I am a fetcher looking for the class variables implemented in a class.
"
Class {
	#name : 'CoClassVariableFetcher',
	#superclass : 'CoClassBasedFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoClassVariableFetcher >> entriesDo: aBlock [

	self completionClass instanceSide classVarNames do: [ :e |
		aBlock value: (CoClassVarEntry contents: e node: astNode)]
]
