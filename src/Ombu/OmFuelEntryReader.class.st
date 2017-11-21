"
I implement how to read entries from a stream using Fuel serializer.
"
Class {
	#name : #OmFuelEntryReader,
	#superclass : #OmEntryReader,
	#instVars : [
		'materializer'
	],
	#category : #Ombu-Persistence
}

{ #category : #enumerating }
OmFuelEntryReader >> entryPositionsDo: aBlock [

	self shouldBeImplemented
]

{ #category : #initialization }
OmFuelEntryReader >> initialize [
	super initialize.
	materializer := FLMaterializer new.
]

{ #category : #reading }
OmFuelEntryReader >> nextEntry [

	^ (materializer materializeFrom: stream) root
]
