Extension { #name : #SycMoveMethodsToClassSideCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycMoveMethodsToClassSideCommand class >> methodMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyMethod asCalypsoItemContext 
]
