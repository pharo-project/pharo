Extension { #name : #SycMoveMethodsToPackageDefiningClassCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycMoveMethodsToPackageDefiningClassCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 80 for: ClyMethod asCalypsoItemContext
]
