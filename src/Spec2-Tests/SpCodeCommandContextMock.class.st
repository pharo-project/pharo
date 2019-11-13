Class {
	#name : #SpCodeCommandContextMock,
	#superclass : #Object,
	#instVars : [
		'selection'
	],
	#category : #'Spec2-Tests-Commands'
}

{ #category : #accessing }
SpCodeCommandContextMock >> selection [

	^ selection
]

{ #category : #accessing }
SpCodeCommandContextMock >> selection: anObject [

	selection := anObject
]
