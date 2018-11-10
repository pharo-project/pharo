"
I open the edit dialog on the credential.
I colaborate with the credential to know the specific class of the dialog model.
Check #editModelClass in IceSshCredentials and IcePlaintextCredentials
"
Class {
	#name : #IceTipEditCredentialCommand,
	#superclass : #IceTipAbstractCredentialCommand,
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #accessing }
IceTipEditCredentialCommand class >> browserShortcutActivation [
	<classAnnotation>

	^ CmdShortcutActivation by: $e meta for: IceTipCredentialContext
]

{ #category : #accessing }
IceTipEditCredentialCommand class >> credentialsBrowserActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 20 for: IceTipCredentialContext

]

{ #category : #accessing }
IceTipEditCredentialCommand class >> defaultHelp [

	^ 'Edits the credential from the store'
]

{ #category : #accessing }
IceTipEditCredentialCommand class >> defaultMenuIconName [

	^ #edit
]

{ #category : #accessing }
IceTipEditCredentialCommand class >> defaultMenuItemName [

	^ 'Edit'
]

{ #category : #execution }
IceTipEditCredentialCommand >> execute [

	credential editModelClass new
		credentials: credential;
		credentialStore: store;
		tool: tool;
		openNonModal.
]
