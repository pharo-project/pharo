Extension { #name : 'SycMoveMethodsToPackageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToPackageCommand class >> methodBrowserDragAndDropActivation [
	<classAnnotation>

	^CmdDragAndDropActivation
		for: ClyMethod asCalypsoItemContext
		toDropIn: Package asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycMoveMethodsToPackageCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 70 for: ClyMethod asCalypsoItemContext
]
