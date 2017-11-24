"
I implement how to write entries to a stream using Fuel serializer.
"
Class {
	#name : #OmFuelEntryWriter,
	#superclass : #OmEntryWriter,
	#instVars : [
		'serializer'
	],
	#category : #'Ombu-Persistence'
}

{ #category : #initialization }
OmFuelEntryWriter >> initialize [
	super initialize.
	serializer := FLSerializer new
]

{ #category : #writing }
OmFuelEntryWriter >> on: aStream nextEntryPut: anEntry [ 

	serializer
		serialize: anEntry 
		on: aStream
]
