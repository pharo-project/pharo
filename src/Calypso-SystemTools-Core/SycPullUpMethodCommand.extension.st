Extension { #name : 'SycPullUpMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycPullUpMethodCommand classSide >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 10 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycPullUpMethodCommand classSide >> methodShortcutActivation [
	
	self flag: 'Uncomment when SycPushUpMethodCommand has been removed'
	"
	<classAnnotation>
	^CmdShortcutActivation by: $p meta, $u meta for: ClyMethod asCalypsoItemContext"
]
