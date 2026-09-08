Extension { #name : 'SycRenameArgOrTempCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameArgOrTempCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameArgOrTempCommand class >> sourceCodeShortcutActivation [
"	<classAnnotation>"
	"Disabled to make the rename shortcut trigger the new solution."

	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
