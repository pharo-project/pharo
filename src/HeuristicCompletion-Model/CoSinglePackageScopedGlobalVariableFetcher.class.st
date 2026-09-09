"
I am a fetcher looking for globals in an environment.
"
Class {
	#name : 'CoSinglePackageScopedGlobalVariableFetcher',
	#superclass : 'CoPackageScopedGlobalVariableFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoSinglePackageScopedGlobalVariableFetcher >> entriesDo: aBlock [

	completionClass ifNil: [ ^ self ].
	self packageLocalClasses: aBlock
]
