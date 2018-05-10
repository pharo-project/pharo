"
I am an abstract superclass for errors that may occur during filesystem operations.
"
Class {
	#name : #FileSystemError,
	#superclass : #Error,
	#instVars : [
		'reference'
	],
	#category : #'FileSystem-Core-Kernel-Errors'
}

{ #category : #'instance creation' }
FileSystemError class >> reference: aReference [
	^ self basicNew initializeWithReference: aReference
]

{ #category : #'instance creation' }
FileSystemError class >> signalWith: aReference [
	^ (self reference: aReference) signal
]

{ #category : #'initialize-release' }
FileSystemError >> initializeWithReference: aReference [
	reference := aReference.
	messageText := aReference printString
]

{ #category : #testing }
FileSystemError >> isResumable [
	^ true
]

{ #category : #accessing }
FileSystemError >> reference [
	^ reference
]
