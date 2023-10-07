Extension { #name : 'SycRemoveAllSendersCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRemoveAllSendersCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation
		byRootGroupItemOrder: 35 for: ClyMethod asCalypsoItemContext
]
