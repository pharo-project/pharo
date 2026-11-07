Extension { #name : 'SycRenameArgOrTempCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameArgOrTempCommand class >> sourceCodeShortcutActivation [
"	<classAnnotation>"
	"Disabled to make the rename shortcut trigger the new solution."

	^ClyShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
