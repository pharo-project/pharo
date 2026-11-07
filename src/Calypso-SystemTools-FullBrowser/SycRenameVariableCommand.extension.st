Extension { #name : 'SycRenameVariableCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRenameVariableCommand class >> fullBrowserShortcutActivation [

	<classAnnotation>
	^ ClyShortcutActivation renamingFor: ClyFullBrowserVariableContext
]
