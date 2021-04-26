Class {
	#name : #ClyBrowserMock,
	#superclass : #Object,
	#instVars : [
		'hasSpawned',
		'elements'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-Tests-Commands-Classes'
}

{ #category : #testing }
ClyBrowserMock >> hasSpawnedQueryBrowser [

	^hasSpawned 
]

{ #category : #testing }
ClyBrowserMock >> hasSpawnedQueryBrowserOn: aCollection [

	^hasSpawned and: [ elements resultItems asArray = aCollection asArray ]
]

{ #category : #initialization }
ClyBrowserMock >> initialize [

	hasSpawned := false
]

{ #category : #mocking }
ClyBrowserMock >> spawnQueryBrowserOn: aCollection [

	hasSpawned := true.
	elements := aCollection
]
