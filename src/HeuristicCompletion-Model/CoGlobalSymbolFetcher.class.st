"
I am a fetcher that gets all symbols. 
I use the SystemNavigation to get all symbols and then filtering them.
See the #entriesDo:
"
Class {
	#name : 'CoGlobalSymbolFetcher',
	#superclass : 'CoGlobalFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoGlobalSymbolFetcher >> entriesDo: aBlock [

	self systemNavigation
		allSymbolsStartingWith: filter completionString
		do: [ :e | aBlock value: (NECSymbolEntry contents: e node: astNode) ]
]
