"
I represent an environment for the completion.
I know a system dictionary, and can be initialized with for example, Smalltalk globals.
"
Class {
	#name : 'CoSystemEnvironment',
	#superclass : 'Object',
	#instVars : [
		'systemDictionary'
	],
	#category : 'HeuristicCompletion-Model-SystemIntegration',
	#package : 'HeuristicCompletion-Model',
	#tag : 'SystemIntegration'
}

{ #category : 'instance creation' }
CoSystemEnvironment class >> forSmalltalkGlobals [

	^ super new
		systemDictionary: Smalltalk globals;
		yourself
]

{ #category : 'instance creation' }
CoSystemEnvironment class >> new [

	^ self forSmalltalkGlobals
]

{ #category : 'accessing' }
CoSystemEnvironment >> at: aString ifPresent: aBlockClosure [

	systemDictionary at: aString ifPresent: aBlockClosure
]

{ #category : 'accessing' }
CoSystemEnvironment >> environmentAt: aKey ifPresent: aBlock [

	^ systemDictionary at: aKey ifPresent: aBlock
]

{ #category : 'accessing' }
CoSystemEnvironment >> globals [

	^ systemDictionary keys sorted
]

{ #category : 'accessing' }
CoSystemEnvironment >> systemDictionary: aDictionary [

	systemDictionary := aDictionary
]
