Extension { #name : 'SycPullUpMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycPullUpMethodCommand class >> methodShortcutActivation [
	
	<classAnnotation>
	^ClyShortcutActivation by: $p meta, $u meta for: ClyMethod asCalypsoItemContext
]
