Extension { #name : 'SycClassifyMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycClassifyMethodCommand class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemFor: ClyMethod asCalypsoItemContext
]
