"
I am a fetcher that yields the messages implemented in the given package.
"
Class {
	#name : #CoPackageImplementedMessagesFetcher,
	#superclass : #CoFetcher,
	#instVars : [
		'completionPackage'
	],
	#category : #'HeuristicCompletion-Fetchers'
}

{ #category : #accessing }
CoPackageImplementedMessagesFetcher >> completionPackage [
	^ completionPackage
]

{ #category : #accessing }
CoPackageImplementedMessagesFetcher >> completionPackage: anObject [
	completionPackage := anObject
]

{ #category : #accessing }
CoPackageImplementedMessagesFetcher >> entriesDo: aBlock [
	
	SystemNavigation new allSentMessagesInPackage: completionPackage do: [ :e |
		aBlock value: (NECSelectorEntry contents: e node: self)]
]
