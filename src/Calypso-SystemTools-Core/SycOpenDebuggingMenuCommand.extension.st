Extension { #name : 'SycOpenDebuggingMenuCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycOpenDebuggingMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $g meta shift for: ClySourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycOpenDebuggingMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^ClyContextMenuActivation byRootGroupItemOrder: 0.2 for: ClySourceCodeContext
]
