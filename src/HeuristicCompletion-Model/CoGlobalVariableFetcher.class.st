"
I am a fetcher looking for globals in an environment.
"
Class {
	#name : #CoGlobalVariableFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'astNode',
		'systemNavigation'
	],
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #enumerating }
CoGlobalVariableFetcher >> acceptsEntry: anEntry [

	"entries are pre-filtered"
	^ true
]

{ #category : #accessing }
CoGlobalVariableFetcher >> astNode [
	^ astNode
]

{ #category : #accessing }
CoGlobalVariableFetcher >> astNode: anObject [
	astNode := anObject
]

{ #category : #enumerating }
CoGlobalVariableFetcher >> entriesDo: aBlock [

	self systemNavigation
		allClassesStartingWith: filter completionString asLowercase 
		do: [ :e | aBlock value: (NECGlobalEntry contents: e node: astNode) ]
]

{ #category : #accessing }
CoGlobalVariableFetcher >> systemNavigation [
	^ systemNavigation ifNil: [ SystemNavigation new ]
]

{ #category : #accessing }
CoGlobalVariableFetcher >> systemNavigation: anObject [
	systemNavigation := anObject
]
