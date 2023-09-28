Extension { #name : 'SycOpenMethodMenuCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycOpenMethodMenuCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10100 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycOpenMethodMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $t meta for: ClyMethod asCalypsoItemContext
]
