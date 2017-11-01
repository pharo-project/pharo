"
I store a username and password.

"
Class {
	#name : #MCServerCredentials,
	#superclass : #Object,
	#instVars : [
		'username',
		'password'
	],
	#category : 'MonticelloRemoteRepositories'
}

{ #category : #'instance creation' }
MCServerCredentials class >> user: nameString password: passwordString [

	^ self new
			username: nameString;
			password: passwordString;
			yourself.
]

{ #category : #accessing }
MCServerCredentials >> password [

	^ password.
]

{ #category : #private }
MCServerCredentials >> password: aString [ 

	password := aString.
]

{ #category : #accessing }
MCServerCredentials >> username [

	^ username.
]

{ #category : #private }
MCServerCredentials >> username: aString [ 

	username := aString.
]
