"
A fetcher that relies on a `SystemNavigation`
"
Class {
	#name : 'CoGlobalFetcher',
	#superclass : 'CoFetcher',
	#instVars : [
		'astNode',
		'systemNavigation'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'accessing' }
CoGlobalFetcher >> astNode [
	^ astNode
]

{ #category : 'accessing' }
CoGlobalFetcher >> astNode: anObject [
	astNode := anObject
]

{ #category : 'accessing' }
CoGlobalFetcher >> systemNavigation [
	^ systemNavigation ifNil: [ SystemNavigation new ]
]

{ #category : 'accessing' }
CoGlobalFetcher >> systemNavigation: anObject [
	systemNavigation := anObject
]
