Extension { #name : 'SycRenameArgOrTempCommand2' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameArgOrTempCommand2 class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameArgOrTempCommand2 class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
