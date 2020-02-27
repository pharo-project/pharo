"
STONTestUser is used to support unit tests.

Instance Variables
	enabled:		<Boolean>
	password:	<String>
	username:	<String>

"
Class {
	#name : #STONTestUser,
	#superclass : #Object,
	#instVars : [
		'username',
		'password',
		'enabled'
	],
	#category : #'STON-Tests-Support'
}

{ #category : #'instance creation' }
STONTestUser class >> dummy [
	"self dummy"
	
	| username password |
	username := String streamContents: [ :stream |
		stream << 'user'; print: 999 atRandom; << '@company'; print: 99 atRandom; << '.com' ].
	password := String streamContents: [ :stream |
		stream << 'secret'; print: 999 atRandom ].
	^ self new
		username: username;
		password: password;
		yourself
]

{ #category : #'ston-core' }
STONTestUser class >> stonName [
	^ #TestUser
]

{ #category : #comparing }
STONTestUser >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ username = anObject username and: [ password = anObject password and: [ enabled = anObject enabled ] ]
]

{ #category : #accessing }
STONTestUser >> enabled [
	^ enabled
]

{ #category : #accessing }
STONTestUser >> enabled: anObject [
	enabled := anObject
]

{ #category : #comparing }
STONTestUser >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ username hash bitXor: (password hash bitXor: enabled hash)
]

{ #category : #'initialize-release' }
STONTestUser >> initialize [ 
	super initialize.
	enabled := true
]

{ #category : #accessing }
STONTestUser >> password [
	^ password
]

{ #category : #accessing }
STONTestUser >> password: anObject [
	password := anObject
]

{ #category : #accessing }
STONTestUser >> username [
	^ username
]

{ #category : #accessing }
STONTestUser >> username: anObject [
	username := anObject
]
