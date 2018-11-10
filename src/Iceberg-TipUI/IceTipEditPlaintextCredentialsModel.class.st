"
I implement the dialog to edit plaintext credentials.
"
Class {
	#name : #IceTipEditPlaintextCredentialsModel,
	#superclass : #IceTipAbstractPlaintextCredentialsModel,
	#instVars : [
		'tool',
		'credential'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #specs }
IceTipEditPlaintextCredentialsModel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed newColumn: [ :col |
		col 
		 	newRow: [:row | row add: #usernameLabel width: 80 * World displayScaleFactor; add: #usernameInput ] height: self inputTextHeight;
			newRow: [:row | row add: #passwordLabel width: 80 * World displayScaleFactor; add: #passwordInput ] height: self inputTextHeight;
			newRow: [:row |]
	]

]

{ #category : #actions }
IceTipEditPlaintextCredentialsModel >> cancelAction [
	" I do not nothing "
]

{ #category : #accessing }
IceTipEditPlaintextCredentialsModel >> credentials [
	^ credential
]

{ #category : #accessing }
IceTipEditPlaintextCredentialsModel >> credentials: aPlainTextCredential [ 

	credential := aPlainTextCredential.
	
	self usernameInput text: credential username.
	self passwordInput text: credential password.
	
]

{ #category : #api }
IceTipEditPlaintextCredentialsModel >> initialExtent [
	^ (350 @ (self class inputTextHeight * 4 + self class buttonHeight)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipEditPlaintextCredentialsModel >> initializeWidgets [
	super initializeWidgets.
	passwordInput encrypted: false 
]

{ #category : #actions }
IceTipEditPlaintextCredentialsModel >> okAction [
	self credentials
		username: self username;
		password: self password.
	
	credentialStore storeCredential: self credentials.
	tool refresh
]

{ #category : #initialization }
IceTipEditPlaintextCredentialsModel >> putFocusOrder [

	self focusOrder
		add: usernameInput;
		add: passwordInput.
]

{ #category : #accessing }
IceTipEditPlaintextCredentialsModel >> title [
	^ 'Edit credential for ' , credential host
]

{ #category : #accessing }
IceTipEditPlaintextCredentialsModel >> tool: aBrowser [ 
	tool := aBrowser
]
