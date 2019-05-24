Extension { #name : #SycMoveMethodsToPackageCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycMoveMethodsToPackageCommand class >> methodBrowserDragAndDropActivation [
	<classAnnotation>
	
	^CmdDragAndDropActivation 
		for: ClyMethod asCalypsoItemContext 
		toDropIn: RPackage asCalypsoItemContext
]
