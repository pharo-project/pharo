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
	
	(self fileName: aFile basename) signal
]

{ #category : #'instance creation' }
FileException class >> signalWith: aReference [
	"Signal a new instance of the receiver with the supplied reference.
	aReference is something that can be converted to a path, e.g. a String, Path or FileReference"

	^(self fileName: aReference asPath pathString) signal
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

	^ messageText ifNil: [ fileName printString ] ifNotNil: [ messageText ]
]
