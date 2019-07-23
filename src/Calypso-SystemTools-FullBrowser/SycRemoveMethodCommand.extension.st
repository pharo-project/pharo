Extension { #name : #SycRemoveMethodCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdOtherOptionsMenuGroup  
		order: 20  
		for: ClyMethod asCalypsoItemContext  
]
