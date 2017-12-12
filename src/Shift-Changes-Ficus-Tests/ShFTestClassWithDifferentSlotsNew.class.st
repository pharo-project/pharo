Class {
	#name : #ShFTestClassWithDifferentSlotsNew,
	#superclass : #Object,
	#instVars : [
		'#oneSlot => PropertySlot'
	],
	#category : 'Shift-Changes-Ficus-Tests-TestClasses'
}

{ #category : #initialization }
ShFTestClassWithDifferentSlotsNew >> initialize [
	super initialize.
	self class initializeSlots: self.
]

{ #category : #accessing }
ShFTestClassWithDifferentSlotsNew >> oneSlot [
	^ oneSlot
]

{ #category : #accessing }
ShFTestClassWithDifferentSlotsNew >> oneSlot: anObject [
	oneSlot := anObject
]
