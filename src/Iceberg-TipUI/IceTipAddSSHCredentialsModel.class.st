"
I implement the dialog to add SSH credentials.
"
Class {
	#name : #IceTipAddSSHCredentialsModel,
	#superclass : #IceTipAbstractSSHCredentialsModel,
	#instVars : [
		'hostLabel',
		'hostInput'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #specs }
IceTipAddSSHCredentialsModel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed newColumn: [ :col |
		col 
		 	newRow: [:row | row add: #hostLabel width: self iceLabelWidth; add: #hostInput ] height: self inputTextHeight;
		 	newRow: [:row | row add: #usernameLabel width: self iceLabelWidth; add: #usernameInput ] height: self inputTextHeight;
			newRow: [:row | row add: #publicKeyInput ] height: self inputTextHeight;
			newRow: [:row | row add: #privateKeyInput ] height: self inputTextHeight;
			newRow: [:row | row add: #passphraseLabel width: self iceLabelWidth; add: #passphraseInput ] height: self inputTextHeight;
			newRow: [:row | ]
	]

]

{ #category : #actions }
IceTipAddSSHCredentialsModel >> cancelAction [
	" I do nothing on cancel "
]

{ #category : #accessing }
IceTipAddSSHCredentialsModel >> credentials [
	
	^ IceSshCredentials new
		username: self username;
		publicKey: self publicKey;
		privateKey: self privateKey;
		keyPassphrase: self passphrase;
		yourself 
]

{ #category : #accessing }
IceTipAddSSHCredentialsModel >> host [
	
	^ self hostInput text
]

{ #category : #accessing }
IceTipAddSSHCredentialsModel >> hostInput [
	^ hostInput
]

{ #category : #accessing }
IceTipAddSSHCredentialsModel >> hostLabel [
	^ hostLabel
]

{ #category : #api }
IceTipAddSSHCredentialsModel >> initialExtent [
	^ (350 @ (self class inputTextHeight * 7 + self class buttonHeight)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipAddSSHCredentialsModel >> initializeWidgets [
	super initializeWidgets.
	
	hostLabel := self newLabel label: 'Host: '.
	hostInput := self newTextInput 
		autoAccept: true;
		whenTextChanged: [ self updateOkButton ];
		yourself.

]

{ #category : #actions }
IceTipAddSSHCredentialsModel >> okAction [
	
	credentialStore storeCredential: self credentials forHostname: self host.
	tool refresh.
]

{ #category : #initialization }
IceTipAddSSHCredentialsModel >> putFocusOrder [
	self focusOrder
		add: hostInput;
		add: usernameInput;
		add: publicKeyInput locationInput;
		add: privateKeyInput locationInput;
		add: passphraseInput;
		add: storeCheckbox
]

{ #category : #api }
IceTipAddSSHCredentialsModel >> title [

	^ 'Add SSH credential'
]
