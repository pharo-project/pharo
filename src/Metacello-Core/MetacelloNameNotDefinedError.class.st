Class {
	#name : #MetacelloNameNotDefinedError,
	#superclass : #Error,
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #accessing }
MetacelloNameNotDefinedError >> isResumable [
  "Pharo and Squeak compatibility"

  ^ true
]
