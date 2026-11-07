Extension { #name : 'SycRenameVariableCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameVariableCommand class >> sourceCodeShortcutActivation [

	<classAnnotation>
	^ ClyShortcutActivation renamingFor: ClySourceCodeContext
]
