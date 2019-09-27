Class {
	#name : #FFITestType,
	#superclass : #FFIExternalType,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #converting }
FFITestType >> asExternalTypeOn: aTypeResolver [
	
	"FFI sends this to us expecting us to be a class most of the times, but for testing purposes, we use an instance."
	^ self
]
