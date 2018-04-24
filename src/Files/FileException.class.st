"
Common superclass for exceptions while using file streams
"
Class {
	#name : #FileException,
	#superclass : #Error,
	#instVars : [
		'fileName'
	],
	#category : #'Files-Core'
}

{ #category : #exceptioninstantiator }
FileException class >> fileName: aFileName [
	^self new fileName: aFileName
]

{ #category : #'instance creation' }
FileException class >> signalOnFile: aFile [ 
	
	(self fileName: aFile name) signal
]

{ #category : #exceptiondescription }
FileException >> fileName [
	^fileName
]

{ #category : #exceptionbuilder }
FileException >> fileName: aFileName [
	fileName := aFileName
]

{ #category : #exceptiondescription }
FileException >> isResumable [
	"Determine whether an exception is resumable."

	^true
]

{ #category : #exceptiondescription }
FileException >> messageText [
	"Return an exception's message text."

	^ messageText isNil
		ifTrue: [ fileName printString ]
		ifFalse: [ messageText ]
]
