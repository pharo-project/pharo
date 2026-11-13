Extension { #name : 'SycOpenMethodMenuCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycOpenMethodMenuCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ClyContextMenuActivation byRootGroupItemOrder: 10100 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycOpenMethodMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $t meta for: ClyMethod asCalypsoItemContext
]
