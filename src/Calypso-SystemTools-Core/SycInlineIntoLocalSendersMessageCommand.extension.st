Extension { #name : 'SycInlineIntoLocalSendersMessageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycInlineIntoLocalSendersMessageCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation
		byRootGroupItemOrder: 35 for: ClyMethod asCalypsoItemContext
]
