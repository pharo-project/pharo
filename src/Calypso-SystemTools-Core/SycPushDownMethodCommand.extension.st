Extension { #name : 'SycPushDownMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycPushDownMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 20 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycPushDownMethodCommand class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $p meta, $d meta for: ClyMethod asCalypsoItemContext
]
