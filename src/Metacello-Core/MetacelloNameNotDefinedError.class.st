Class {
	#name : #MetacelloNameNotDefinedError,
	#superclass : #Error,
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #Accessing }
MetacelloNameNotDefinedError >> isResumable [
  "Pharo and Squeak compatibility"

  ^ true
]
