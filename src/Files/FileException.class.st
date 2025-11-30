"
Common superclass for exceptions while using file streams
"
Class {
	#name : 'FileException',
	#superclass : 'Error',
	#instVars : [
		'entity'
	],
	#category : 'Files-Core',
	#package : 'Files',
	#tag : 'Core'
}

{ #category : 'instance creation' }
FileException class >> fileName: aFileName [

	^ self new entity: aFileName
]

{ #category : 'instance creation' }
FileException class >> reference: aReference [

	^ self new
		  entity: aReference;
		  messageText: aReference printString
]

{ #category : 'instance creation' }
FileException class >> signalOnFile: aFile [

	(self fileName: aFile basename) signal
]

{ #category : 'instance creation' }
FileException class >> signalWith: aReference [
	"Signal a new instance of the receiver with the supplied reference.
	aReference is something that can be converted to a path, e.g. a String, Path or FileReference"

	^ (self reference: aReference) signal
]

{ #category : 'accessing' }
FileException >> entity [

	^ entity
]

{ #category : 'accessing' }
FileException >> entity: anObject [

	entity := anObject
]

{ #category : 'testing' }
FileException >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]

{ #category : 'accessing' }
FileException >> messageText [
	"Return an exception's message text."

	^ messageText ifNil: [ self entity printString ]
]
