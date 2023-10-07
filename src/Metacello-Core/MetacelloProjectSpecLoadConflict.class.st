Class {
	#name : 'MetacelloProjectSpecLoadConflict',
	#superclass : 'MetacelloProjectSpecLoadError',
	#category : 'Metacello-Core-Exceptions-Errors',
	#package : 'Metacello-Core',
	#tag : 'Exceptions-Errors'
}

{ #category : 'private - testing' }
MetacelloProjectSpecLoadConflict >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]

{ #category : 'accessing' }
MetacelloProjectSpecLoadConflict >> possibleVersions [
    "not applicable to a Conflict error"

    self shouldNotImplement
]

{ #category : 'accessing' }
MetacelloProjectSpecLoadConflict >> versionString [
    "not applicable to a Conflict error"

    self shouldNotImplement
]
