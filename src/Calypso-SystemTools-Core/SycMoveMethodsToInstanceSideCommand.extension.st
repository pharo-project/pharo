Extension { #name : 'SycMoveMethodsToInstanceSideCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToInstanceSideCommand class >> methodShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $t meta, $i meta for: ClyMethod asCalypsoItemContext
]
