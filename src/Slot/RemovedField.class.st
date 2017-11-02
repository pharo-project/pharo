"
I represent the change of a removed field on an instance.
"
Class {
	#name : #RemovedField,
	#superclass : #AbstractFieldModification,
	#instVars : [
		'originalSlot'
	],
	#category : 'Slot-ClassBuilder'
}

{ #category : #installing }
RemovedField >> installOn: aModification [
	aModification installRemovedSlot: self
]

{ #category : #accessing }
RemovedField >> oldFieldIndex [
	^ originalSlot index + fieldIndex
]

{ #category : #accessing }
RemovedField >> originalSlot [
	^ originalSlot
]

{ #category : #accessing }
RemovedField >> originalSlot: anObject [
	originalSlot := anObject
]
