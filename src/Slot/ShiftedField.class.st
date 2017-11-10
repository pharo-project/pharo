"
I represent the change of a shifted field on an instance.
The old field and the new field correspond to the same Slot.

In terms of method migration, it is only necessary to update the index of direct accessors/bytecodes to instance variable.
"
Class {
	#name : #ShiftedField,
	#superclass : #AbstractFieldModification,
	#instVars : [
		'shift'
	],
	#category : #Slot-ClassBuilder
}

{ #category : #migrating }
ShiftedField >> migrateAt: index to: newInstance from: oldInstance [
	newInstance 
		instVarAt: index 
		put: (oldInstance instVarAt: index - shift)
]

{ #category : #accessing }
ShiftedField >> shift [
	^ shift
]

{ #category : #accessing }
ShiftedField >> shift: anObject [
	shift := anObject
]
