"
I am a fetcher looking for the variables defined in the shared pools of the current class
"
Class {
	#name : 'CoSharedPoolVariableFetcher',
	#superclass : 'CoClassBasedFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoSharedPoolVariableFetcher >> entriesDo: aBlock [

	self completionClass instanceSide sharedPools do: [ :e |
		e allClassVarNames do: [ :name | aBlock value: (NECClassVarEntry contents: name node: astNode) ] ]
]
