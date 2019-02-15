"
I am a buffer of keyboard events. I am checked against key combinations to see if there is a match.

I am cleared when the current morph loses focus or when a full match announcement is given.
"
Class {
	#name : #KMBuffer,
	#superclass : #Object,
	#instVars : [
		'buffer',
		'currentEvent'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Keymapping-Core-Base'
}

{ #category : #'initialize-release' }
KMBuffer class >> resetUniqueInstance [
	uniqueInstance := nil 
]

{ #category : #accessing }
KMBuffer class >> uniqueInstance [
	^uniqueInstance ifNil: [ uniqueInstance := self new ].
]

{ #category : #accessing }
KMBuffer class >> uniqueInstance: aBuffer [
	uniqueInstance := aBuffer.
]

{ #category : #adding }
KMBuffer >> addEvent: anEvent [

	buffer add: anEvent.
	currentEvent := anEvent.
]

{ #category : #accessing }
KMBuffer >> buffer [
	^buffer
]

{ #category : #clearing }
KMBuffer >> clearBuffer [

	currentEvent := nil.
	buffer removeAll.
]

{ #category : #matching }
KMBuffer >> completeMatch [

	currentEvent ifNotNil:[ currentEvent wasHandled: true ].
	self clearBuffer.
]

{ #category : #initialization }
KMBuffer >> initialize [
	buffer := OrderedCollection new.
]

{ #category : #matching }
KMBuffer >> partialMatch [

	KMLog logPartialMatch: currentEvent.
	currentEvent ifNotNil:[ currentEvent wasHandled: true ].
]
