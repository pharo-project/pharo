Extension { #name : 'SycPushUpMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycPushUpMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 10 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycPushUpMethodCommand class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $p meta, $u meta for: ClyMethod asCalypsoItemContext
]
