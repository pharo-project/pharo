Extension { #name : 'SycRenameVariableCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRenameVariableCommand class >> fullBrowserMenuActivation [

	<classAnnotation>
	^ CmdContextMenuActivation
		  byRootGroupItemOrder: 1
		  for: ClyFullBrowserVariableContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRenameVariableCommand class >> fullBrowserShortcutActivation [

	<classAnnotation>
	^ CmdShortcutActivation renamingFor: ClyFullBrowserVariableContext
]
