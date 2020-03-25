"
I am a fetcher looking for globals in an environment.
"
Class {
	#name : #CoGlobalVariableFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'completionEnvironment'
	],
	#category : #'HeuristicCompletion-Fetchers'
}

{ #category : #accessing }
CoGlobalVariableFetcher >> completionEnvironment [
	^ completionEnvironment
]

{ #category : #accessing }
CoGlobalVariableFetcher >> completionEnvironment: anObject [
	completionEnvironment := anObject
]

{ #category : #accessing }
CoGlobalVariableFetcher >> entriesDo: aBlock [

	completionEnvironment globals do: [ :e |
		aBlock value: (NECGlobalEntry contents: e node: self)]
]
