"
I add a new IceSshCredentials to the store
"
Class {
	#name : #IceTipAddSshCredentialCommand,
	#superclass : #IceTipAbstractCredentialStoreCommand,
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #activation }
IceTipAddSshCredentialCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipCredentialsStoreContext
		order: 100
]

{ #category : #accessing }
IceTipAddSshCredentialCommand class >> defaultHelp [
	^ 'Add a SSH pair of credential'
]

{ #category : #accessing }
IceTipAddSshCredentialCommand class >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
IceTipAddSshCredentialCommand class >> defaultMenuItemName [
	^ 'SSH Pair'
]

{ #category : #accessing }
IceTipAddSshCredentialCommand class >> defaultPosition [
	^ #right
]

{ #category : #execution }
IceTipAddSshCredentialCommand >> execute [
	IceTipAddSSHCredentialsModel new
		credentialStore: store;
		tool: tool;
		openNonModal.
]
