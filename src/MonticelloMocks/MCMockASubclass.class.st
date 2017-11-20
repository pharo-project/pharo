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

{ #category : #'as yet unclassified' }
MCMockASubclass class >> initialize [

	InitializationOrder := InitializationOrder
		ifNil: [ -100 ] "let the test fail"
		ifNotNil: [ InitializationOrder + 1.]
]

{ #category : #'as yet unclassified' }
MCMockASubclass >> variables [
	^ x + Y + MCMockClassA
]

{ #category : #'as yet unclassified' }
MCMockASubclass >> variables2 [
	^ ivar + CVar
]
