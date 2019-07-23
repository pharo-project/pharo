Extension { #name : #SycMoveMethodsToInstanceSideCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycMoveMethodsToInstanceSideCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdOtherOptionsMenuGroup  
		order: 17  
		for: ClyMethod asCalypsoItemContext  
]
