"
I implement how to write entries to a stream using Ston serializer.
"
Class {
	#name : #OmSTONEntryWriter,
	#superclass : #OmEntryWriter,
	#instVars : [
		'writer'
	],
	#category : #Ombu-Persistence
}

{ #category : #initialization }
OmSTONEntryWriter >> initialize [
	super initialize.
	writer := self newSTONWriter
]

{ #category : #initialization }
OmSTONEntryWriter >> newSTONWriter [
	^ STON writer
		prettyPrint: true;
		yourself
]

{ #category : #writing }
OmSTONEntryWriter >> on: aStream nextEntryPut: anEntry [ 

	OmCurrentGlobalName
		value: store globalName
		during: [
			writer 
				reset;
				on: aStream;
				nextPut: anEntry.

			aStream cr;cr ]
]
