Class {
	#name : #MetacelloCannotUpdateReleasedVersionError,
	#superclass : #Error,
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #private }
MetacelloCannotUpdateReleasedVersionError >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]
