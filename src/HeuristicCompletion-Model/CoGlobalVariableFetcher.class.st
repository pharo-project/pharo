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

	NECPreferences camelCaseGlobalMatching
		ifTrue: [ ^ self entriesDoWithFullEnvironmentScan: aBlock ].
		
	self systemNavigation
		allGlobalNamesStartingWith: filter completionString
		do: [ :e | aBlock value: (self newEntryFor: e) ]
		caseSensitive: NECPreferences caseSensitive

]

{ #category : 'enumerating' }
CoGlobalVariableFetcher >> entriesDoWithFullEnvironmentScan: aBlock [

	self systemNavigation
		allGlobalNamesStartingWith: ''
		do: [ :e | aBlock value: (self newEntryFor: e) ]
		caseSensitive: true
]

{ #category : 'private' }
CoGlobalVariableFetcher >> newEntryFor: aGlobalName [

	^ (NECGlobalEntry contents: aGlobalName node: astNode)
		fetcher: self;
		yourself
]
