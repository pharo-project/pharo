"
I create a new plaintext credential on the credential store.
"
Class {
	#name : #IceTipAddPlainTextCredentialCommand,
	#superclass : #IceTipAbstractCredentialStoreCommand,
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #activation }
IceTipAddPlainTextCredentialCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipCredentialsStoreContext
		order: 100
]

{ #category : #accessing }
IceTipAddPlainTextCredentialCommand class >> defaultHelp [
	^ 'Add a new plain text credential'
]

{ #category : #accessing }
IceTipAddPlainTextCredentialCommand class >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
IceTipAddPlainTextCredentialCommand class >> defaultMenuItemName [
	^ 'Plain Text'
]

{ #category : #accessing }
IceTipAddPlainTextCredentialCommand class >> defaultPosition [
	^ #right
]

{ #category : #execution }
IceTipAddPlainTextCredentialCommand >> execute [
	IceTipAddPlaintextCredentialsModel new
		credentialStore: store;
		tool: tool;
		openNonModal.
]
