Extension { #name : #SycRenameLocalVariableCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameLocalVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameLocalVariableCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
