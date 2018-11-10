"
I implement the basic behavior of all the ssh credentials.
My subclasses implements the specific behavior for each of the operations.
"
Class {
	#name : #IceTipAbstractSSHCredentialsModel,
	#superclass : #IceTipAbstractAskCredentialsModel,
	#instVars : [
		'usernameInput',
		'usernameLabel',
		'publicKeyInput',
		'privateKeyInput',
		'passphraseInput',
		'passphraseLabel',
		'tool'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #initialization }
IceTipAbstractSSHCredentialsModel >> initializeWidgets [
	super initializeWidgets.

	usernameLabel := self newLabel label: 'Username: '.
	usernameInput := self newTextInput
		text: 'git';
		autoAccept: true;
		whenTextChanged: [ self updateOkButton ];
		yourself.

	passphraseLabel := self newLabel label: 'Key Passphrase: '.
	passphraseInput := self newTextInput
		beEncrypted;
		autoAccept: true;
		whenTextChanged: [ self updateOkButton ];
		yourself.
		
	privateKeyInput := IceTipFilePresenter new 
		label: 'Private Key';
		onChoose: [ self updateOkButton ];
		chooseTitle: 'Select Private Key';
		whenTextChanged: [ self updateOkButton ];
		yourself.
		
	publicKeyInput := IceTipFilePresenter new 
		label: 'Public Key';
		onChoose: [ self updateOkButton ];
		chooseTitle: 'Select Public Key';
		whenTextChanged: [ self updateOkButton ];		
		yourself.	
]

{ #category : #testing }
IceTipAbstractSSHCredentialsModel >> isOkEnabled [
	
	^ self username isNotEmpty and: [ self publicKey isNotEmpty and: [ self privateKey isNotEmpty ] ]
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> passphrase [
	^ passphraseInput text
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> passphraseInput [
	^ passphraseInput
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> passphraseLabel [
	^ passphraseLabel
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> privateKey [
	^ privateKeyInput location ifNil: [ '' ] ifNotNil: [ :x | x pathString ]
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> privateKeyInput [
	^ privateKeyInput
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> publicKey [
	^ publicKeyInput location ifNil: [ '' ] ifNotNil: [ :x | x pathString ]
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> publicKeyInput [
	^ publicKeyInput
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> tool: aBrowser [ 
	tool := aBrowser
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> username [
	^ usernameInput text
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> usernameInput [
	^ usernameInput
]

{ #category : #accessing }
IceTipAbstractSSHCredentialsModel >> usernameLabel [
	^ usernameLabel
]
