Extension { #name : #SycOpenAdvancedDebuggingMenuCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenAdvancedDebuggingMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $h meta shift for: ClySourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenAdvancedDebuggingMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 8 for: ClySourceCodeContext
]
