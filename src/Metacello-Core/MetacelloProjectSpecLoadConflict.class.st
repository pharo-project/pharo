Class {
	#name : #MetacelloProjectSpecLoadConflict,
	#superclass : #MetacelloProjectSpecLoadError,
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #private }
MetacelloProjectSpecLoadConflict >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]

{ #category : #accessing }
MetacelloProjectSpecLoadConflict >> possibleVersions [
    "not applicable to a Conflict error"

    self shouldNotImplement
]

{ #category : #accessing }
MetacelloProjectSpecLoadConflict >> versionString [
    "not applicable to a Conflict error"

    self shouldNotImplement
]
