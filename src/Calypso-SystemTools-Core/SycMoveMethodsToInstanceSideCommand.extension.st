Extension { #name : #SycMoveMethodsToInstanceSideCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycMoveMethodsToInstanceSideCommand class >> methodMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyMethod asCalypsoItemContext
]
