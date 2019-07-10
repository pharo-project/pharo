Extension { #name : #SycRenameTempCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameTempCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameTempCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
