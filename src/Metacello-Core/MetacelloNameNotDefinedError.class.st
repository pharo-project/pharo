Class {
	#name : #MetacelloNameNotDefinedError,
	#superclass : #Error,
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #testing }
MetacelloNameNotDefinedError >> isResumable [
	"Pharo and Squeak compatibility"

	^ true
]
