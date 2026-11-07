Extension { #name : 'SycMoveMethodsToClassCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToClassCommand class >> methodBrowserDragAndDropActivation [
	<classAnnotation>

	^ClyDragAndDropActivation
		for: ClyMethod asCalypsoItemContext toDropIn: ClyClass asCalypsoItemContext
]
