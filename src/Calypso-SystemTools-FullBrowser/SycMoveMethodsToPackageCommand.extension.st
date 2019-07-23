Extension { #name : #SycMoveMethodsToPackageCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycMoveMethodsToPackageCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdOtherOptionsMenuGroup  
		order: 19  
		for: ClyMethod asCalypsoItemContext  
]
