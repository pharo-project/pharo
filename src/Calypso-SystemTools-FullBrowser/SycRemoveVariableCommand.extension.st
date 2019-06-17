Extension { #name : #SycRemoveVariableCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyVariableContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveVariableCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation removalFor: ClyVariableContextOfFullBrowser
]
