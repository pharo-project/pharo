Extension { #name : 'SycMoveMethodsToClassSideCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToClassSideCommand class >> methodShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $t meta, $c meta for: ClyMethod asCalypsoItemContext
]
