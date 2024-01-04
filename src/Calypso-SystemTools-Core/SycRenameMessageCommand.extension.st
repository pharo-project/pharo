Extension { #name : 'SycRenameMessageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageCommand class >> methodMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageCommand class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethod asCalypsoItemContext
]
