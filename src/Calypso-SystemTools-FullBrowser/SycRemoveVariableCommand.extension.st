Extension { #name : #SycRemoveVariableCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyFullBrowserVariableContext
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveVariableCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyFullBrowserVariableContext
]
