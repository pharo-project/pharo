"
I am a command to force full update of all browser views
"
Class {
	#name : #ClyUpdateBrowserCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-Browser-Commands'
}

{ #category : #activations }
ClyUpdateBrowserCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyBrowserContext
]

{ #category : #accessing }
ClyUpdateBrowserCommand >> defaultMenuIconName [
	^#smallUpdate
]

{ #category : #accessing }
ClyUpdateBrowserCommand >> defaultMenuItemName [
	^'Update'
]

{ #category : #execution }
ClyUpdateBrowserCommand >> execute [
	browser update
]
