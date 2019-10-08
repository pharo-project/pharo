Class {
	#name : #FFITestType,
	#superclass : #FFIExternalType,
	#instVars : [
		'type'
	],
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #'instance creation' }
FFITestType class >> of: aType [

	^ self new type: aType; yourself
]

{ #category : #converting }
FFITestType >> asExternalTypeOn: aTypeResolver [
	
	"FFI sends this to us expecting us to be a class most of the times, but for testing purposes, we use an instance."
	^ self copy
]

{ #category : #accessing }
FFITestType >> externalType [
	
	^ type externalType
]

{ #category : #accessing }
FFITestType >> type: anObject [
	type := anObject
]
