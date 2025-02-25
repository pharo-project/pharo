Extension { #name : 'SycInlineAllSendersMessageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycInlineAllSendersMessageCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation
		byRootGroupItemOrder: 35 for: ClyMethod asCalypsoItemContext
]
