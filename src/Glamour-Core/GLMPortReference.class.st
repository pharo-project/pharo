"
A PortReference is used in Transmissions to bahavior that is specific to a Transmission instance.
"
Class {
	#name : #GLMPortReference,
	#superclass : #Object,
	#instVars : [
		'port'
	],
	#category : #Glamour-Core
}

{ #category : #accessing }
GLMPortReference >> port [ 
	 
	^port
]

{ #category : #accessing }
GLMPortReference >> port: aPort [
	port := aPort
]

{ #category : #copying }
GLMPortReference >> postCopy [ 
	port := port copy
]

{ #category : #printing }
GLMPortReference >> printOn: aStream [
	super printOn: aStream.
	aStream 
		nextPut: Character space;
		nextPutAll: self identityHash printString;
		nextPutAll: ' ('.
	port printOn: aStream.
	aStream nextPut: $)
]
