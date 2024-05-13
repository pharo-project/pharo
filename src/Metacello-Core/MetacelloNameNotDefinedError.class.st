Class {
	#name : 'MetacelloNameNotDefinedError',
	#superclass : 'MetacelloError',
	#category : 'Metacello-Core-Exceptions-Errors',
	#package : 'Metacello-Core',
	#tag : 'Exceptions-Errors'
}

{ #category : 'testing' }
MetacelloNameNotDefinedError >> isResumable [
	"Pharo and Squeak compatibility"

	^ true
]
