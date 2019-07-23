Extension { #name : #SycRenameMessageCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRenameMessageCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdRefactorMenuGroup  
		order: 1  
		for: ClyMethod asCalypsoItemContext  
]
