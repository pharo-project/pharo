Extension { #name : #SycRenameTempCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameTempCommand classSide >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameTempCommand classSide >> sourceCodeShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
