"
I implement the basic behavior of all the plaintext credentials.
My subclasses implements the specific behavior for each of the operations.
"
Class {
	#name : #IceTipAbstractPlaintextCredentialsModel,
	#superclass : #IceTipAbstractAskCredentialsModel,
	#instVars : [
		'usernameLabel',
		'usernameInput',
		'passwordLabel',
		'passwordInput'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #accessing }
IceTipAbstractPlaintextCredentialsModel >> credentials [
	^ IcePlaintextCredentials new 
		username: self username;
		password: self password;
		yourself.

]

{ #category : #initialization }
IceTipAbstractPlaintextCredentialsModel >> initializeWidgets [
	super initializeWidgets.

	usernameLabel := self newLabel label: 'Username: '.
	usernameInput := self newTextInput
		autoAccept: true;
		whenTextChanged: [ self updateOkButton ];
		yourself.

	passwordLabel := self newLabel label: 'Password: '.
	passwordInput := self newTextInput
		beEncrypted;
		autoAccept: true;
		whenTextChanged: [ self updateOkButton ];
		yourself.
]

{ #category : #testing }
IceTipAbstractPlaintextCredentialsModel >> isOkEnabled [
	
	^ self username isNotEmpty and: [self password isNotEmpty]
]

{ #category : #accessing }
IceTipAbstractPlaintextCredentialsModel >> password [ 
	^ passwordInput text
]

{ #category : #accessing }
IceTipAbstractPlaintextCredentialsModel >> passwordInput [
	^ passwordInput
]

{ #category : #accessing }
IceTipAbstractPlaintextCredentialsModel >> passwordLabel [
	^ passwordLabel
]

{ #category : #accessing }
IceTipAbstractPlaintextCredentialsModel >> username [
	^ usernameInput text
]

{ #category : #accessing }
IceTipAbstractPlaintextCredentialsModel >> usernameInput [
	^ usernameInput
]

{ #category : #accessing }
IceTipAbstractPlaintextCredentialsModel >> usernameLabel [
	^ usernameLabel
]
