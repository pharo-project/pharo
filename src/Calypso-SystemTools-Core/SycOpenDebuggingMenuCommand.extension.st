Extension { #name : #SycOpenDebuggingMenuCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenDebuggingMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $g meta shift for: ClySourceCodeContext 
]

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenDebuggingMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: -1 for: ClySourceCodeContext
]
