Class {
	#name : #MetacelloProjectSpecLoadError,
	#superclass : #Error,
	#instVars : [
		'projectSpec',
		'versionDoesNotExistException'
	],
	#category : #Metacello-Core-Exceptions-Errors
}

{ #category : #'instance creation' }
MetacelloProjectSpecLoadError class >> projectSpec: aMetacelloMCProjectSpec [

	^self new
		projectSpec: aMetacelloMCProjectSpec;
		yourself
]

{ #category : #private }
MetacelloProjectSpecLoadError >> isResumable [
	"Determine whether an exception is resumable."

	^ false
]

{ #category : #accessing }
MetacelloProjectSpecLoadError >> possibleVersions [
    ^ self versionDoesNotExistException possibleVersions
]

{ #category : #accessing }
MetacelloProjectSpecLoadError >> projectSpec [

	^projectSpec
]

{ #category : #accessing }
MetacelloProjectSpecLoadError >> projectSpec: aMetacelloMCProjectSpec [

	projectSpec := aMetacelloMCProjectSpec
]

{ #category : #accessing }
MetacelloProjectSpecLoadError >> versionDoesNotExistException [
	^ versionDoesNotExistException
]

{ #category : #accessing }
MetacelloProjectSpecLoadError >> versionDoesNotExistException: anObject [
	versionDoesNotExistException := anObject
]

{ #category : #accessing }
MetacelloProjectSpecLoadError >> versionString [
    ^ self versionDoesNotExistException versionString
]
