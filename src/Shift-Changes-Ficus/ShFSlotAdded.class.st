Class {
	#name : #ShFSlotAdded,
	#superclass : #ShFSlotChange,
	#instVars : [
		'slotName',
		'slotType'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #accessing }
ShFSlotAdded >> slotName [
	^ slotName
]

{ #category : #accessing }
ShFSlotAdded >> slotName: anObject [
	slotName := anObject
]

{ #category : #accessing }
ShFSlotAdded >> slotType [
	^ slotType
]

{ #category : #accessing }
ShFSlotAdded >> slotType: anObject [
	slotType := anObject
]
