Class {
	#name : 'MetacelloNameNotDefinedError',
	#superclass : 'MetacelloError',
	#category : 'Metacello-Core-Exceptions',
	#package : 'Metacello-Core',
	#tag : 'Exceptions'
}

{ #category : 'testing' }
MetacelloNameNotDefinedError >> isResumable [
	"Pharo and Squeak compatibility"

	^ true
]
