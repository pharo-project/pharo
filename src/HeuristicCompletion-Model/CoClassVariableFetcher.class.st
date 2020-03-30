Class {
	#name : #CoClassVariableFetcher,
	#superclass : #CoClassBasedFetcher,
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #enumerating }
CoClassVariableFetcher >> entriesDo: aBlock [
	
	self completionClass instanceSide classVarNames do: [ :e |
		aBlock value: (NECClassVarEntry contents: e node: self)]
]
