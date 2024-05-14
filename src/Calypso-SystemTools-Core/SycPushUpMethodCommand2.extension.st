Extension { #name : 'SycPushUpMethodCommand2' }

{ #category : '*Calypso-SystemTools-Core' }
SycPushUpMethodCommand2 class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 10 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycPushUpMethodCommand2 class >> methodShortcutActivation [
	
	self flag: 'Uncomment when SycPushUpMethodCommand has been removed'
	"
	<classAnnotation>
	^CmdShortcutActivation by: $p meta, $u meta for: ClyMethod asCalypsoItemContext"
]
