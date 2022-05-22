"
I am a fetcher that gets all message selectors. 
I use the SystemNavigation to get all selectors and then filtering them.
See the #entriesDo:
"
Class {
	#name : #CoGlobalSelectorFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'astNode',
		'systemNavigation'
	],
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #accessing }
CoGlobalSelectorFetcher >> astNode [
	^ astNode
]

{ #category : #accessing }
CoGlobalSelectorFetcher >> astNode: anObject [
	astNode := anObject
]

{ #category : #enumerating }
CoGlobalSelectorFetcher >> entriesDo: aBlock [

	self systemNavigation
		allSelectorsStartingWith: filter completionString 
		do: [ :e | aBlock value: (NECSelectorEntry contents: e node: astNode) ]
]

{ #category : #accessing }
CoGlobalSelectorFetcher >> systemNavigation [
	^ systemNavigation ifNil: [ SystemNavigation new ]
]

{ #category : #accessing }
CoGlobalSelectorFetcher >> systemNavigation: anObject [
	systemNavigation := anObject
]
