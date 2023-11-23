Extension { #name : 'SycDeprecateMessageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycDeprecateMessageCommand class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation
		byRootGroupItemOrder: self browserMenuOrder for: ClyMethod asCalypsoItemContext
]
