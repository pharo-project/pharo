"
I am used to prompt in a UI indepentent way for a username and a password.
If the user successfully entered the two strings I return an MCServerCredential if not I return nil.
"
Class {
	#name : #MCCredentialsRequest,
	#superclass : #Notification,
	#instVars : [
		'username',
		'password',
		'url'
	],
	#category : 'MonticelloRemoteRepositories'
}

{ #category : #signalling }
MCCredentialsRequest class >> signalUrl: aUrl [
	^ self new 
		url: aUrl;
		signal
]

{ #category : #signalling }
MCCredentialsRequest class >> signalUrl: aUrl username: username password: password [
	^ self new 
		url: aUrl;
		username: username;
		password: password;
		signal
]

{ #category : #exceptiondescription }
MCCredentialsRequest >> defaultAction [
	
	username := UIManager default
			request: self userMessagePrompt
			initialAnswer: self username
			title: 'User' translated.
	(username isNil or: [ username isEmpty ])
		ifTrue: [ ^ nil ].
	password := UIManager default
			requestPassword: self passwordMessagePrompt.
	^ MCServerCredentials user: username password: password
]

{ #category : #accessing }
MCCredentialsRequest >> password [
	^ password
]

{ #category : #accessing }
MCCredentialsRequest >> password: anObject [
	password := anObject
]

{ #category : #accessing }
MCCredentialsRequest >> passwordMessagePrompt [
	^ 'Please enter a password for ', self url asString
]

{ #category : #accessing }
MCCredentialsRequest >> url [
	^ url
]

{ #category : #accessing }
MCCredentialsRequest >> url: anObject [
	url := anObject
]

{ #category : #accessing }
MCCredentialsRequest >> userMessagePrompt [
	^ 'Please enter a username for ', self url asString
]

{ #category : #accessing }
MCCredentialsRequest >> username [
	^ username
]

{ #category : #accessing }
MCCredentialsRequest >> username: anObject [
	username := anObject
]
