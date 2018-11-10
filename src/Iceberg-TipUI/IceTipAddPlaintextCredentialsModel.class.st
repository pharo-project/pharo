"
I implement the dialog to add plaintext credentials.
"
Class {
	#name : #IceTipAddPlaintextCredentialsModel,
	#superclass : #IceTipAbstractPlaintextCredentialsModel,
	#instVars : [
		'hostLabel',
		'hostInput',
		'tool'
	],
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #specs }
IceTipAddPlaintextCredentialsModel class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed newColumn: [ :col |
		col 
		 	newRow: [:row | row add: #hostLabel width: 80 * World displayScaleFactor; add: #hostInput ] height: self inputTextHeight;
		 	newRow: [:row | row add: #usernameLabel width: 80 * World displayScaleFactor; add: #usernameInput ] height: self inputTextHeight;
			newRow: [:row | row add: #passwordLabel width: 80 * World displayScaleFactor; add: #passwordInput ] height: self inputTextHeight;
			newRow: [:row | ]
	]

]

{ #category : #testing }
IceTipAddPlaintextCredentialsModel >> cancelAction [
	"I do not produce any error"
]

{ #category : #accessing }
IceTipAddPlaintextCredentialsModel >> host [
	^ self hostInput text
]

{ #category : #accessing }
IceTipAddPlaintextCredentialsModel >> hostInput [
	^ hostInput
]

{ #category : #accessing }
IceTipAddPlaintextCredentialsModel >> hostLabel [
	^ hostLabel
]

{ #category : #initialization }
IceTipAddPlaintextCredentialsModel >> initialExtent [
	^ (350 @ (self class inputTextHeight * 5 + self class buttonHeight)) scaledByDisplayScaleFactor
]

{ #category : #initialization }
IceTipAddPlaintextCredentialsModel >> initializeWidgets [

	super initializeWidgets.

	hostLabel := self newLabel label: 'Host: '.
	hostInput := self newTextInput 
		autoAccept: true;
		whenTextChanged: [ self updateOkButton ];
		yourself.


]

{ #category : #testing }
IceTipAddPlaintextCredentialsModel >> isOkEnabled [
	^ super isOkEnabled and: [ self host isNotEmpty ]
]

{ #category : #testing }
IceTipAddPlaintextCredentialsModel >> okAction [
	
	credentialStore storeCredential: self credentials forHostname: self host.
	tool refresh.
]

{ #category : #initialization }
IceTipAddPlaintextCredentialsModel >> putFocusOrder [
	self focusOrder
		add: hostInput;
		add: usernameInput;
		add: passwordInput;
		add: storeCheckbox
]

{ #category : #accessing }
IceTipAddPlaintextCredentialsModel >> title [
	^ 'Add new Plain Text Credential'
]

{ #category : #accessing }
IceTipAddPlaintextCredentialsModel >> tool: anObject [ 
	tool := anObject
]
