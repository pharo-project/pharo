Class {
	#name : #IceCloneError,
	#superclass : #IceError,
	#category : #'Iceberg-Libgit-Exceptions'
}

{ #category : #signalling }
IceCloneError class >> signalFor: anObject [

	^ self subclassResponsibility
]
