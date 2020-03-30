"
I am a fetcher looking for globals in an environment.
"
Class {
	#name : #CoGlobalVariableFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'completionEnvironment',
		'astNode'
	],
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #accessing }
CoGlobalVariableFetcher >> astNode [
	^ astNode
]

{ #category : #accessing }
CoGlobalVariableFetcher >> astNode: anObject [
	astNode := anObject
]

{ #category : #accessing }
CoGlobalVariableFetcher >> completionEnvironment [
	^ completionEnvironment
]

{ #category : #accessing }
CoGlobalVariableFetcher >> completionEnvironment: anObject [
	completionEnvironment := anObject
]

{ #category : #enumerating }
CoGlobalVariableFetcher >> entriesDo: aBlock [

	completionEnvironment globals do: [ :e |
		aBlock value: (NECGlobalEntry contents: e node: astNode)]
]
