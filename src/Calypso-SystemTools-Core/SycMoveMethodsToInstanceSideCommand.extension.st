Extension { #name : 'SycMoveMethodsToInstanceSideCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToInstanceSideCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 50 for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToInstanceSideCommand class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $t meta, $i meta for: ClyMethod asCalypsoItemContext
]
