"
Base class for all errors in Iceberg.
"
Class {
	#name : #IceError,
	#superclass : #Error,
	#category : #'Iceberg-Errors'
}

{ #category : #visiting }
IceError >> acceptError: aVisitor [
	aVisitor visitGenericError: self
]
