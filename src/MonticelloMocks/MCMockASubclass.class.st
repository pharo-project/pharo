Class {
	#name : #MCMockASubclass,
	#superclass : #MCMockClassA,
	#instVars : [
		'x'
	],
	#classVars : [
		'Y'
	],
	#category : #MonticelloMocks
}

{ #category : #'class initialization' }
MCMockASubclass class >> initialize [

	InitializationOrder := InitializationOrder
		ifNil: [ -100 ] "let the test fail"
		ifNotNil: [ InitializationOrder + 1.]
]

{ #category : #accessing }
MCMockASubclass >> variables [
	^ x + Y + MCMockClassA
]

{ #category : #accessing }
MCMockASubclass >> variables2 [
	^ ivar + CVar
]
