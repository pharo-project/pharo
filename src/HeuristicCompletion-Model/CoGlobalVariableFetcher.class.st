Class {
	#name : #CoGlobalVariableFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'completionEnvironment'
	],
	#category : #'HeuristicCompletion-Model-Fetchers'
}

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
		aBlock value: (NECGlobalEntry contents: e node: self)]
]
