"
I am a fetcher looking for the instance variables implemented in a class.
"
Class {
	#name : 'CoInstanceVariableFetcher',
	#superclass : 'CoClassBasedFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoInstanceVariableFetcher >> entriesDo: aBlock [

	self completionClass instVarNames do: [ :e |
		aBlock value: (NECInstVarEntry contents: e node: astNode)]
]
