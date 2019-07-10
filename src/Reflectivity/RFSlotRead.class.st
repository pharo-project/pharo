"
I model the read from a slot
"
Class {
	#name : #RFSlotRead,
	#superclass : #RFVariableRead,
	#instVars : [
		'object'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFSlotRead >> object [
	^ object
]

{ #category : #accessing }
RFSlotRead >> object: anObject [
	object := anObject
]

{ #category : #evaluating }
RFSlotRead >> value [
	^variable read: object
]

{ #category : #accessing }
RFSlotRead >> variableName: aSymbol [
	variable := object class slotNamed: aSymbol
]
