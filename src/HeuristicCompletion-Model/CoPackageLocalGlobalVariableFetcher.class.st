Class {
	#name : 'CoPackageLocalGlobalVariableFetcher',
	#superclass : 'CoPackageScopedGlobalVariableFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoPackageLocalGlobalVariableFetcher >> entriesDo: aBlock [

	self packageLocalClasses: aBlock
]
