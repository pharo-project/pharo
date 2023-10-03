Extension { #name : 'SycRemoveMessageArgumentCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRemoveMessageArgumentCommand class >> browserMenuOrder [
	^2.2
]

{ #category : '*Calypso-SystemTools-Core' }
SycRemoveMessageArgumentCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 40 for: ClyMethod asCalypsoItemContext
]
