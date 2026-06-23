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

	self systemNavigation allSymbolsDo: [ :e |
			aBlock value: ((CoSymbolEntry contents: e node: astNode)
					 fetcherName: self class name;
					 yourself) ]
]
