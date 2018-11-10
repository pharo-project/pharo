"
I keep a username and a password for connecting to a repository.
"
Class {
	#name : #IcePlaintextCredentials,
	#superclass : #IceAbstractCredentials,
	#instVars : [
		'username',
		'password'
	],
	#category : #'Iceberg-Security'
}

{ #category : #printing }
IcePlaintextCredentials >> description [
	^ 'User: ' , username
]

{ #category : #printing }
IcePlaintextCredentials >> hostDescription [
	^ host
]

{ #category : #testing }
IcePlaintextCredentials >> isPresent [
	^ self username isEmpty not and: [ self password isEmpty not ]
]

{ #category : #accessing }
IcePlaintextCredentials >> password [
	^ password ifNil: [ password := '' ]
]

{ #category : #accessing }
IcePlaintextCredentials >> password: anObject [
	password := anObject
]

{ #category : #'instance creation' }
IcePlaintextCredentials >> readFrom: credentials [
	self username: credentials username.
	self password: credentials password
]

{ #category : #'API - storing' }
IcePlaintextCredentials >> removeFrom: aStore [ 
	
	aStore removePlainTextCredential: self
]

{ #category : #'API - storing' }
IcePlaintextCredentials >> storeInto: aCredentialStore forHostname: aHost [ 
	host := aHost.
	aCredentialStore storePlaintextCredential: self forHostname: aHost
]

{ #category : #accessing }
IcePlaintextCredentials >> type [
	^ 'Plain Text'
]

{ #category : #accessing }
IcePlaintextCredentials >> username [
	^ username ifNil: [ username := '' ]
]

{ #category : #accessing }
IcePlaintextCredentials >> username: anObject [
	username := anObject
]
