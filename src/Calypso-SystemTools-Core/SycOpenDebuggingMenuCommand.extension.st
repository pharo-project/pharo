Extension { #name : #SycOpenDebuggingMenuCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenDebuggingMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $t meta for: ClySourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenDebuggingMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: -1 for: ClySourceCodeContext
]
