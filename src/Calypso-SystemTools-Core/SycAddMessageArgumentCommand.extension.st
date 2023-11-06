Extension { #name : 'SycAddMessageArgumentCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycAddMessageArgumentCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation
		byRootGroupItemOrder: 30 for: ClyMethod asCalypsoItemContext
]
