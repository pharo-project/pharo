"
I am ZnBasicCredential, a concrete subclass of ZnCredential.
I implement basic HTTP authentication.
I hold a username and password.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnBasicCredential,
	#superclass : #ZnCredential,
	#instVars : [
		'username',
		'password'
	],
	#category : #Zinc-HTTP-Support
}

{ #category : #accessing }
ZnBasicCredential class >> authName [
	^ 'Basic'
]

{ #category : #accessing }
ZnBasicCredential >> authTokenForUrl: urlObject method: method [
	^ 'Basic ', (ZnUtils encodeBase64: (username, ':', password)) 
]

{ #category : #testing }
ZnBasicCredential >> isComplete [ 
	^ username notNil & password notNil 
]

{ #category : #accessing }
ZnBasicCredential >> password [
	^ password
]

{ #category : #accessing }
ZnBasicCredential >> password: anObject [
	password := anObject
]

{ #category : #accessing }
ZnBasicCredential >> username [
	^ username
]

{ #category : #accessing }
ZnBasicCredential >> username: anObject [
	username := anObject
]
