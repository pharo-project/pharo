Extension { #name : #SycMoveMethodsToClassPackageCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycMoveMethodsToClassPackageCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdOtherOptionsMenuGroup  
		order: 18  
		for: ClyMethod asCalypsoItemContext  
]
