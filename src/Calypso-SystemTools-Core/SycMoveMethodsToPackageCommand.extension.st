Extension { #name : 'SycMoveMethodsToPackageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToPackageCommand class >> methodBrowserDragAndDropActivation [
	<classAnnotation>

	^ClyDragAndDropActivation
		for: ClyMethod asCalypsoItemContext
		toDropIn: Package asCalypsoItemContext
]
