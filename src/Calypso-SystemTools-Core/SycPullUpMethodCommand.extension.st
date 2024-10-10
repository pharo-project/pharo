Extension { #name : 'SycPullUpMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycPullUpMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 10 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycPullUpMethodCommand class >> methodShortcutActivation [
	
	<classAnnotation>
	^CmdShortcutActivation by: $p meta, $u meta for: ClyMethod asCalypsoItemContext
]
