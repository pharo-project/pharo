"
I am a simple mock for tests
"
Class {
	#name : 'ObjectMockForTest',
	#superclass : 'Object',
	#instVars : [
		'variable'
	],
	#category : 'Kernel-Tests-Objects',
	#package : 'Kernel-Tests',
	#tag : 'Objects'
}

{ #category : 'accessing' }
ObjectMockForTest >> variable [
	^ variable
]

{ #category : 'accessing' }
ObjectMockForTest >> variable: anObject [
	variable := anObject
]
