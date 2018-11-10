"
I'm an error to be sent when the error that comes from external library can be guessed to be the absense of user.name and user.email properties.
"
Class {
	#name : #IceGitUsernameOrEmailNotFound,
	#superclass : #IceError,
	#category : #'Iceberg-Errors'
}

{ #category : #accessing }
IceGitUsernameOrEmailNotFound class >> guessKind: anError ifNot: notBlock [ 
	(anError messageText asLowercase = 'Config value ''user.name'' was not found' asLowercase
		or: [ anError messageText asLowercase = 'Config value ''user.email'' was not found' asLowercase
		or: [ anError messageText asLowercase = 'Failed to parse signature - Signature cannot have an empty name or email' asLowercase ] ])
		ifTrue: [ ^ self signal ].
	notBlock value
]

{ #category : #visiting }
IceGitUsernameOrEmailNotFound >> acceptError: anObject [
	anObject visitMissingCredentialsError: self
]

{ #category : #visiting }
IceGitUsernameOrEmailNotFound >> isResumable [

	^ true
]
