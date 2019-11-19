"
STONTestAssociation is used to support unit tests.

"
Class {
	#name : #STONTestAssociation,
	#superclass : #Association,
	#instVars : [
		'timestamp'
	],
	#category : #'STON-Tests-Support'
}

{ #category : #accessing }
STONTestAssociation >> key: aKey value: anObject [ 
	super key: aKey value: anObject.
	timestamp := self now
]

{ #category : #accessing }
STONTestAssociation >> now [
	^ DateAndTime now
]

{ #category : #accessing }
STONTestAssociation >> timestamp [
	^ timestamp
]

{ #category : #accessing }
STONTestAssociation >> value: anObject [
	super value: anObject.
	timestamp := self now
]
