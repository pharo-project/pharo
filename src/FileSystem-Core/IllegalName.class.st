"
I am raised on an attempt to use an illegal file name

"
Class {
	#name : #IllegalName,
	#superclass : #FileSystemError,
	#instVars : [
		'name'
	],
	#category : #'FileSystem-Core-Kernel-Errors'
}

{ #category : #'instance creation' }
IllegalName class >> name: aName [
	^ self basicNew initializeWithName: aName
]

{ #category : #'instance creation' }
IllegalName class >> signalWith: aName [
	^ (self name: aName) signal
]

{ #category : #initialization }
IllegalName >> initializeWithName: aName [
	name := aName.
	self messageText: aName
]

{ #category : #accessing }
IllegalName >> name [
	^ name
]
