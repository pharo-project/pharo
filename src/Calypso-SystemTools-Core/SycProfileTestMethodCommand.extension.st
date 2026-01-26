Extension { #name : 'SycProfileTestMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycProfileTestMethodCommand class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemFor: ClyMethod asCalypsoItemContext
]
