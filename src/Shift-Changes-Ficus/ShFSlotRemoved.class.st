Class {
	#name : #ShFSlotRemoved,
	#superclass : #ShFSlotChange,
	#instVars : [
		'slotName',
		'slotType'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #accessing }
ShFSlotRemoved >> slotName [
	^ slotName
]

{ #category : #accessing }
ShFSlotRemoved >> slotName: anObject [
	slotName := anObject
]

{ #category : #accessing }
ShFSlotRemoved >> slotType [
	^ slotType
]

{ #category : #accessing }
ShFSlotRemoved >> slotType: anObject [
	slotType := anObject
]
