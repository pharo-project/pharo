"
I am a fetcher that yields the messages implemented in the given package.
"
Class {
	#name : 'CoPackageImplementedMessagesFetcher',
	#superclass : 'CoFetcher',
	#instVars : [
		'completionPackage',
		'astNode'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'accessing' }
CoPackageImplementedMessagesFetcher >> astNode [
	^ astNode
]

{ #category : 'accessing' }
CoPackageImplementedMessagesFetcher >> astNode: anObject [
	astNode := anObject
]

{ #category : 'accessing' }
CoPackageImplementedMessagesFetcher >> completionPackage [
	^ completionPackage
]

{ #category : 'accessing' }
CoPackageImplementedMessagesFetcher >> completionPackage: anObject [
	completionPackage := anObject
]

{ #category : 'enumerating' }
CoPackageImplementedMessagesFetcher >> entriesDo: aBlock [

	SystemNavigation new allSentMessagesInPackage: completionPackage do: [ :e |
		aBlock value: (NECSelectorEntry contents: e node: astNode)]
]
