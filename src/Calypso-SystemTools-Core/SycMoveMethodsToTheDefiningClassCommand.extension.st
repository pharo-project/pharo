Extension { #name : 'SycMoveMethodsToTheDefiningClassCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToTheDefiningClassCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 80 for: ClyMethod asCalypsoItemContext
]
