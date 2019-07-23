Extension { #name : #SycRemoveMessageArgumentCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveMessageArgumentCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdOtherOptionsMenuGroup  
		order: 19  
		for: ClyMethod asCalypsoItemContext  
]
