"
I encapsulate the modification of a slot.
The two use-cases are: slot renaming or a changed slot type.
Both cases enforce recompilation of all the methods accessing the corresponding variable.

This is unlike the ShiftedField modification which is only used when the type of the slot does not change.
"
Class {
	#name : #ModifiedField,
	#superclass : #AbstractFieldModification,
	#instVars : [
		'newSlot',
		'originalSlot'
	],
	#category : #'Slot-ClassBuilder'
}

{ #category : #installing }
ModifiedField >> installOn: aModification [
	aModification installModifiedSlot: self
]

{ #category : #migrating }
ModifiedField >> migrateAt: index to: newInstance from: oldInstance [
	newInstance
		instVarAt: index
		put: (oldInstance instVarAt: self oldFieldIndex)
]

{ #category : #accessing }
ModifiedField >> newFieldIndex [
	^ newSlot index + fieldIndex
]

{ #category : #accessing }
ModifiedField >> newSlot [
	^ newSlot
]

{ #category : #accessing }
ModifiedField >> newSlot: anObject [
	newSlot := anObject
]

{ #category : #accessing }
ModifiedField >> oldFieldIndex [
	^ originalSlot index + fieldIndex
]

{ #category : #accessing }
ModifiedField >> originalSlot [

	^ originalSlot
]

{ #category : #accessing }
ModifiedField >> originalSlot: anObject [
	originalSlot := anObject
]
