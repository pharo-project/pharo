"
I am the error raised when one tries add an entry in registry for which the name already exists.
"
Class {
	#name : #CmDuplicatedEntryName,
	#superclass : #Error,
	#instVars : [
		'entryName'
	],
	#category : #'Commander2-Errors'
}

{ #category : #'instance creation' }
CmDuplicatedEntryName class >> forEntryNamed: aString [
	^ self new
		entryName: aString;
		yourself
]

{ #category : #signalling }
CmDuplicatedEntryName class >> signalEntryNamed: aString [
	^ (self forEntryNamed: aString)  signal
]

{ #category : #accessing }
CmDuplicatedEntryName >> entryName [
	^ entryName
]

{ #category : #accessing }
CmDuplicatedEntryName >> entryName: aString [
	entryName := aString
]
