Extension { #name : 'SycPushDownMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycPushDownMethodCommand class >> methodShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $p meta, $d meta for: ClyMethod asCalypsoItemContext
]
