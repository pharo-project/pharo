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

	self systemNavigation
		allGlobalNamesStartingWith: filter completionString
		do: [ :e | aBlock value: (NECGlobalEntry contents: e node: astNode) ]
		caseSensitive: (NECPreferences caseSensitive)
]
