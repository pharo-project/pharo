"
I represent the change of a newly added field on an instance.
"
Class {
	#name : #AddedField,
	#superclass : #AbstractFieldModification,
	#instVars : [
		'newSlot'
	],
	#category : #Slot-ClassBuilder
}

{ #category : #installing }
AddedField >> installOn: aModification [
	aModification installAddedSlot: self
]

{ #category : #accessing }
AddedField >> name [
	^ newSlot name
]

{ #category : #accessing }
AddedField >> newFieldIndex [
	^ newSlot index + fieldIndex
]

{ #category : #accessing }
AddedField >> newSlot [
	^ newSlot
]

{ #category : #accessing }
AddedField >> newSlot: anObject [
	newSlot := anObject
]
