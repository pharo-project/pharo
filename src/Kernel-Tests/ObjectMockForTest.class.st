"
I am a simple mock for tests
"
Class {
	#name : #ObjectMockForTest,
	#superclass : #Object,
	#instVars : [
		'variable'
	],
	#category : 'Kernel-Tests-Objects'
}

{ #category : #accessing }
ObjectMockForTest >> variable [
	^ variable
]

{ #category : #accessing }
ObjectMockForTest >> variable: anObject [
	variable := anObject
]
