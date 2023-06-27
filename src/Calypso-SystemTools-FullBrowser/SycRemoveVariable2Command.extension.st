Extension { #name : #SycRemoveVariable2Command }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveVariable2Command class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyFullBrowserVariableContext
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveVariable2Command class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyFullBrowserVariableContext
]
