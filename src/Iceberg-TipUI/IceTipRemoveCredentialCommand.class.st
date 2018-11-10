"
I remove a credential from the credential store.
"
Class {
	#name : #IceTipRemoveCredentialCommand,
	#superclass : #IceTipAbstractCredentialCommand,
	#category : #'Iceberg-TipUI-Credentials'
}

{ #category : #accessing }
IceTipRemoveCredentialCommand class >> browserShortcutActivation [
	<classAnnotation>

	^ CmdShortcutActivation by: $x meta for: IceTipCredentialContext
]

{ #category : #testing }
IceTipRemoveCredentialCommand class >> canBeExecutedInContext: aCredentialContext [
	
	^ aCredentialContext credential isDefault not
]

{ #category : #accessing }
IceTipRemoveCredentialCommand class >> credentialsBrowserActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 20 for: IceTipCredentialContext

]

{ #category : #accessing }
IceTipRemoveCredentialCommand class >> defaultHelp [

	^ 'Removes the credential from the store'
]

{ #category : #accessing }
IceTipRemoveCredentialCommand class >> defaultMenuIconName [

	^ #remove
]

{ #category : #accessing }
IceTipRemoveCredentialCommand class >> defaultMenuItemName [

	^ 'Remove'
]

{ #category : #execution }
IceTipRemoveCredentialCommand >> execute [
	
	store removeCredential: credential.
	tool refresh.
]
