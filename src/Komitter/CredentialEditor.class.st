"
I am a simple UI used to set the credentials of a remote
"
Class {
	#name : #CredentialEditor,
	#superclass : #ComposablePresenter,
	#instVars : [
		'username',
		'usernameLabel',
		'password',
		'passwordLabel',
		'showPassword'
	],
	#category : 'Komitter-Support'
}

{ #category : #specs }
CredentialEditor class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :col |
			col 
				newRow: [ :r | r add: #usernameLabel width: 80; add: #username ] height: self inputTextHeight;
				newRow: [ :r | r add: #passwordLabel width: 80; add: #password ] height: self inputTextHeight + 4;
				newRow: [ :r | r add: #showPasswordWidget width: 120; newColumn: [ :c | ] ] height: self inputTextHeight + 4;
				newRow: [ :r | ]
		];
	yourself
]

{ #category : #api }
CredentialEditor >> getPassword [

	^ password text
]

{ #category : #api }
CredentialEditor >> getUsername [
	
	^ username text
]

{ #category : #api }
CredentialEditor >> initialExtent [

	^ 400@180
]

{ #category : #initialize-release }
CredentialEditor >> initializeDialogWindow: aWindow [

	self 
		bindKeyCombination: $m meta 
		toAction: [ aWindow triggerOkAction ]
]

{ #category : #initialize-release }
CredentialEditor >> initializePresenter [

	showPassword 
		whenActivatedDo: [ password beDecrypted ];
		whenDeactivatedDo: [ password beEncrypted ]
]

{ #category : #initialize-release }
CredentialEditor >> initializeWidgets [

	username := self newTextInput.
	usernameLabel := self newLabel.
	
	password := self newTextInput.
	passwordLabel := self newLabel.
	
	showPassword := self newCheckBox.
	
	usernameLabel label: 'Username'.
	passwordLabel label: 'Password'.
	
	password
		ghostText: '*****';
		autoAccept: true;
		beEncrypted.
		
	username 
		ghostText: 'John Doe';
		autoAccept: true.
		
	showPassword	
		label: 'Show password'.
		
	self focusOrder
		add: username;
		add: password;
		add: showPassword.
]

{ #category : #accessing }
CredentialEditor >> password [
	^ password
]

{ #category : #accessing }
CredentialEditor >> passwordLabel [
	^ passwordLabel
]

{ #category : #api }
CredentialEditor >> setPassword: aPassword [

	password text: aPassword
]

{ #category : #api }
CredentialEditor >> setUsername: aName [

	username text: aName
]

{ #category : #accessing }
CredentialEditor >> showPasswordWidget [
	^ showPassword
]

{ #category : #api }
CredentialEditor >> title [

	^ 'Edit credentials'
]

{ #category : #accessing }
CredentialEditor >> username [
	^ username
]

{ #category : #accessing }
CredentialEditor >> usernameLabel [
	^ usernameLabel
]
