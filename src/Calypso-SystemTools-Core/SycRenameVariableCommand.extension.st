Extension { #name : 'SycRenameVariableCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameVariableCommand class >> sourceCodeMenuActivation [

	<classAnnotation>
	^ SycSourceCodeMenuActivation
		  byRootGroupItemOrder: 1.1
		  for: ClySourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameVariableCommand class >> sourceCodeShortcutActivation [

	<classAnnotation>
	^ CmdShortcutActivation renamingFor: ClySourceCodeContext
]
