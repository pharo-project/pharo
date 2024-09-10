Extension { #name : 'SycProfileMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycProfileMethodCommand class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemFor: ClyMethod asCalypsoItemContext
]
