Class {
	#name : #CoPackageImplementedMessagesFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'completionPackage'
	],
	#category : #'HeuristicCompletion-Model-Fetchers'
}

{ #category : #accessing }
CoPackageImplementedMessagesFetcher >> completionPackage [
	^ completionPackage
]

{ #category : #accessing }
CoPackageImplementedMessagesFetcher >> completionPackage: anObject [
	completionPackage := anObject
]

{ #category : #enumerating }
CoPackageImplementedMessagesFetcher >> entriesDo: aBlock [
	
	SystemNavigation new allSentMessagesInPackage: completionPackage do: [ :e |
		aBlock value: (NECSelectorEntry contents: e node: self)]
]
