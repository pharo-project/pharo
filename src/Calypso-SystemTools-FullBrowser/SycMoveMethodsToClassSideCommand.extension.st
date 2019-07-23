Extension { #name : #SycMoveMethodsToClassSideCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycMoveMethodsToClassSideCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdOtherOptionsMenuGroup  
		order: 16 
		for: ClyMethod asCalypsoItemContext  
]
