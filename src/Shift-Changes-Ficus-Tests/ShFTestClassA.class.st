"
A Test Class
"
Class {
	#name : #ShFTestClassA,
	#superclass : #Object,
	#instVars : [
		'oneSlot'
	],
	#category : 'Shift-Changes-Ficus-Tests-TestClasses'
}

{ #category : #accessing }
ShFTestClassA >> oneSlot [
	^ oneSlot
]

{ #category : #accessing }
ShFTestClassA >> oneSlot: anObject [
	oneSlot := anObject
]
