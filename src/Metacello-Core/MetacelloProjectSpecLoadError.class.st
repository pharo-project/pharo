Class {
	#name : 'MetacelloProjectSpecLoadError',
	#superclass : 'MetacelloError',
	#instVars : [
		'projectSpec',
		'versionDoesNotExistException'
	],
	#category : 'Metacello-Core-Exceptions-Errors',
	#package : 'Metacello-Core',
	#tag : 'Exceptions-Errors'
}

{ #category : 'instance creation' }
MetacelloProjectSpecLoadError class >> projectSpec: aMetacelloMCProjectSpec [

	^self new
		projectSpec: aMetacelloMCProjectSpec;
		yourself
]

{ #category : 'accessing' }
MetacelloProjectSpecLoadError >> possibleVersions [
    ^ self versionDoesNotExistException possibleVersions
]

{ #category : 'accessing' }
MetacelloProjectSpecLoadError >> projectSpec [

	^projectSpec
]

{ #category : 'accessing' }
MetacelloProjectSpecLoadError >> projectSpec: aMetacelloMCProjectSpec [

	projectSpec := aMetacelloMCProjectSpec
]

{ #category : 'accessing' }
MetacelloProjectSpecLoadError >> versionDoesNotExistException [
	^ versionDoesNotExistException
]

{ #category : 'accessing' }
MetacelloProjectSpecLoadError >> versionDoesNotExistException: anObject [
	versionDoesNotExistException := anObject
]

{ #category : 'accessing' }
MetacelloProjectSpecLoadError >> versionString [
    ^ self versionDoesNotExistException versionString
]
