Class {
	#name : #ShFTestClassANew,
	#superclass : #Object,
	#instVars : [
		'otherSlot',
		'oneSlot'
	],
	#category : 'Shift-Changes-Ficus-Tests-TestClasses'
}

{ #category : #accessing }
ShFTestClassANew >> oneSlot [
	^ oneSlot
]

{ #category : #accessing }
ShFTestClassANew >> oneSlot: anObject [
	oneSlot := anObject
]

{ #category : #accessing }
ShFTestClassANew >> otherSlot [
	^ otherSlot
]

{ #category : #accessing }
ShFTestClassANew >> otherSlot: anObject [
	otherSlot := anObject
]
