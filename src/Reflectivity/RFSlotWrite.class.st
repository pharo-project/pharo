"
I model the write to a global variable
"
Class {
	#name : #RFSlotWrite,
	#superclass : #RFVariableWrite,
	#instVars : [
		'object'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFSlotWrite >> object [
	^ object
]

{ #category : #accessing }
RFSlotWrite >> object: anObject [
	object := anObject
]

{ #category : #accessing }
RFSlotWrite >> value [
	^variable write: assignedValue to: object 
]

{ #category : #accessing }
RFSlotWrite >> variableName: aSymbol [
	variable := object class slotNamed: aSymbol
]
