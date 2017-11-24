"
Ceci n'est pas une modifications.
I am used to migrate non modified fields from the old to the new instance.
Typically I am used when only certain fields are modified and others are left as is.
"
Class {
	#name : #UnmodifiedField,
	#superclass : #AbstractFieldModification,
	#instVars : [
		'slot'
	],
	#category : #'Slot-ClassBuilder'
}

{ #category : #accessing }
UnmodifiedField >> fieldIndex [
	^ slot index + fieldIndex
]

{ #category : #accessing }
UnmodifiedField >> installOn: aModification [
	aModification installUnmodifiedSlot: self
]

{ #category : #migrating }
UnmodifiedField >> migrateAt: index to: newInstance from: oldInstance [
	newInstance
		instVarAt: index
		put: (oldInstance instVarAt: index)
]

{ #category : #accessing }
UnmodifiedField >> slot [
	^ slot
]

{ #category : #accessing }
UnmodifiedField >> slot: anObject [
	slot := anObject
]
