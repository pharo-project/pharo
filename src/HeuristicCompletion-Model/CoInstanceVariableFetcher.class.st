Class {
	#name : #CoInstanceVariableFetcher,
	#superclass : #CoClassBasedFetcher,
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #enumerating }
CoInstanceVariableFetcher >> entriesDo: aBlock [
	
	self completionClass instVarNames do: [ :e |
		aBlock value: (NECInstVarEntry contents: e node: self)]
]
