"
I implement the dialog to ask plaintext credentials.
This dialog is used when the IceTipInteractiveCredentialsProvider needs to ask a plaintext credential.
"
Class {
	#name : #IceTipAskForPlaintextCredentialsModel,
	#superclass : #IceTipAbstractPlaintextCredentialsModel,
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #specs }
IceTipAskForPlaintextCredentialsModel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed newColumn: [ :col |
		col 
		 	newRow: [:row | row add: #usernameLabel width: 80 * World displayScaleFactor; add: #usernameInput ] height: self inputTextHeight;
			newRow: [:row | row add: #passwordLabel width: 80 * World displayScaleFactor; add: #passwordInput ] height: self inputTextHeight;
			add: #storeCheckbox height: self inputTextHeight.
	]

]

{ #category : #actions }
IceTipAskForPlaintextCredentialsModel >> cancelAction [
	accepted := true.
	self window close.
	LGitNoCredentialsProvided signal
]

{ #category : #api }
IceTipAskForPlaintextCredentialsModel >> initialExtent [
	^ (350 @ (self class inputTextHeight * 5 + self class buttonHeight)) scaledByDisplayScaleFactor
]

{ #category : #actions }
IceTipAskForPlaintextCredentialsModel >> okAction [

	| cred |
	cred := self credentials.
	self storeCredentials ifTrue: [credentialStore storeCredential: cred forHostname: remoteHostname ].
	
	accepted := true.

	^ cred.
]

{ #category : #initialization }
IceTipAskForPlaintextCredentialsModel >> putFocusOrder [
	self focusOrder
		add: usernameInput;
		add: passwordInput;
		add: storeCheckbox
]

{ #category : #accessing }
IceTipAskForPlaintextCredentialsModel >> title [
	^ 'Please log in to ' , remoteHostname
]
