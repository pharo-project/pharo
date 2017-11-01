"
I am an abstract field modification.
Field modifications encapsulate the changes required to migrated instances.
"
Class {
	#name : #AbstractFieldModification,
	#superclass : #Object,
	#instVars : [
		'fieldIndex'
	],
	#category : 'Slot-ClassBuilder'
}

{ #category : #accessing }
AbstractFieldModification >> fieldIndex [
	^ fieldIndex
]

{ #category : #accessing }
AbstractFieldModification >> fieldIndex: anObject [
	fieldIndex := anObject
]

{ #category : #installing }
AbstractFieldModification >> installOn: aModification [
	self subclassResponsibility
]

{ #category : #migrating }
AbstractFieldModification >> migrateAt: index to: newInstance from: oldInstance [
]
