Extension { #name : #SycOpenSourceCodeMenuCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenSourceCodeMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $t meta for: ClySourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenSourceCodeMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: -10 for: ClySourceCodeContext
]
