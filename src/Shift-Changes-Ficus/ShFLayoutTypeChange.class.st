Class {
	#name : #ShFLayoutTypeChange,
	#superclass : #ShFAbstractSingleChange,
	#category : 'Shift-Changes-Ficus'
}

{ #category : #'checking change' }
ShFLayoutTypeChange >> valueOf: aDefinition [
	^ aDefinition layout layoutClassName
]
