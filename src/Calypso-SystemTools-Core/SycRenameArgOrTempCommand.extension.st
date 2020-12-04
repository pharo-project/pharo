Extension { #name : #SycRenameArgOrTempCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameArgOrTempCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameArgOrTempCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
