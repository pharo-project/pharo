Extension { #name : #SycMoveMethodsToClassCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycMoveMethodsToClassCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdOtherOptionsMenuGroup  
		order: 15  
		for: ClyMethod asCalypsoItemContext  
]
