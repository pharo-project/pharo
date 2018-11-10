"
Directory does not exist in the specified version in the repository.
"
Class {
	#name : #IceMissingRepositoryEntry,
	#superclass : #IceError,
	#instVars : [
		'name',
		'owner'
	],
	#category : #'Iceberg-Errors'
}

{ #category : #accessing }
IceMissingRepositoryEntry >> messageText [
	^ self name
]

{ #category : #accessing }
IceMissingRepositoryEntry >> name [
	^ name
]

{ #category : #accessing }
IceMissingRepositoryEntry >> name: anObject [
	name := anObject
]

{ #category : #accessing }
IceMissingRepositoryEntry >> owner [
	^ owner
]

{ #category : #accessing }
IceMissingRepositoryEntry >> owner: anObject [
	owner := anObject
]
