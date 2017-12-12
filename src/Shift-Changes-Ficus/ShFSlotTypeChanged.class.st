Class {
	#name : #ShFSlotTypeChanged,
	#superclass : #ShFSlotChange,
	#instVars : [
		'slotName',
		'oldSlotType',
		'newSlotType'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #accessing }
ShFSlotTypeChanged >> newSlotType [
	^ newSlotType
]

{ #category : #accessing }
ShFSlotTypeChanged >> newSlotType: anObject [
	newSlotType := anObject
]

{ #category : #accessing }
ShFSlotTypeChanged >> oldSlotType [
	^ oldSlotType
]

{ #category : #accessing }
ShFSlotTypeChanged >> oldSlotType: anObject [
	oldSlotType := anObject
]

{ #category : #accessing }
ShFSlotTypeChanged >> slotName [
	^ slotName
]

{ #category : #accessing }
ShFSlotTypeChanged >> slotName: anObject [
	slotName := anObject
]
