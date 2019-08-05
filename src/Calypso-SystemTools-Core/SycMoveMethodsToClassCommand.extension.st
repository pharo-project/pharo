Extension { #name : #SycMoveMethodsToClassCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycMoveMethodsToClassCommand class >> methodBrowserDragAndDropActivation [
	<classAnnotation>
	
	^CmdDragAndDropActivation 
		for: ClyMethod asCalypsoItemContext toDropIn: ClyClass asCalypsoItemContext
]
