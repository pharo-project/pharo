"
Notify when fie does not exist
"
Class {
	#name : #FileDoesNotExistException,
	#superclass : #FileException,
	#instVars : [
		'readOnly'
	],
	#category : #'Files-Core'
}

{ #category : #signalling }
FileDoesNotExistException class >> signalWithFile: aFile writeMode: writeMode [

	^ (self fileName: aFile name)
		readOnly: writeMode not;
		signal
]

{ #category : #accessing }
FileDoesNotExistException >> readOnly [
	^readOnly == true
]

{ #category : #accessing }
FileDoesNotExistException >> readOnly: aBoolean [
	readOnly := aBoolean
]
