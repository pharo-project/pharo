Class {
	#name : #CoClassImplementedMessagesFetcher,
	#superclass : #CoClassBasedFetcher,
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #enumerating }
CoClassImplementedMessagesFetcher >> entriesDo: aBlock [
	
	self completionClass selectorsDo: [ :e | 
		aBlock value: (NECSelectorEntry contents: e node: astNode)]
]
