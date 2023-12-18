Extension { #name : 'SycRenameVariableCommand2' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRenameVariableCommand2 class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyFullBrowserVariableContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRenameVariableCommand2 class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyFullBrowserVariableContext
]
