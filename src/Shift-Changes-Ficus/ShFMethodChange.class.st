Class {
	#name : #ShFMethodChange,
	#superclass : #ShFAbstractChange,
	#instVars : [
		'selector',
		'sourceCode'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #accessing }
ShFMethodChange >> selector [
	^ selector
]

{ #category : #accessing }
ShFMethodChange >> selector: anObject [
	selector := anObject
]

{ #category : #accessing }
ShFMethodChange >> sourceCode [
	^ sourceCode
]

{ #category : #accessing }
ShFMethodChange >> sourceCode: anObject [
	sourceCode := anObject
]
