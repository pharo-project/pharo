Extension { #name : #SycAddMessageArgumentCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddMessageArgumentCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byItemOf: CmdRefactorMenuGroup 
		order: 3
		for: ClyMethod asCalypsoItemContext
]
