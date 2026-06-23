"
I am a fetcher looking for globals in an environment.
"
Class {
	#name : 'CoGlobalVariableFetcher',
	#superclass : 'CoGlobalFetcher',
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'enumerating' }
CoGlobalVariableFetcher >> completionClass: aClass [
	"we do not use this information but other alternatives such as the CoPackageGlobalVariable needs the completion class to start with."
	
]

{ #category : 'enumerating' }
CoGlobalVariableFetcher >> entriesDo: aBlock [

	self systemNavigation allGlobalNamesDo: [ :e |
				aBlock value: ((CoGlobalEntry contents: e node: astNode)
						 fetcherName: self class name;
						 yourself) ]
]
