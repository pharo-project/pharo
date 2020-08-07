"
I am a fetcher looking for the class variables implemented in a class.
"
Class {
	#name : #CoClassVariableFetcher,
	#superclass : #CoClassBasedFetcher,
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #enumerating }
CoClassVariableFetcher >> entriesDo: aBlock [
	
	self completionClass ifNotNil: [:cc | cc instanceSide classVarNames do: [ :e |
		aBlock value: (NECClassVarEntry contents: e node: astNode)]]
]
