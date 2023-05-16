"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
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
