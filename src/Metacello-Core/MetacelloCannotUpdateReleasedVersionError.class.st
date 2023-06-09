Class {
	#name : #MetacelloCannotUpdateReleasedVersionError,
	#superclass : #Error,
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #'private - testing' }
MetacelloCannotUpdateReleasedVersionError >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]
