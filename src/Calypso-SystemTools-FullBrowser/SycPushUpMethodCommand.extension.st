Extension { #name : #SycPushUpMethodCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycPushUpMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
		byItemOf: CmdRefactorMenuGroup 
		order: 30 
		for: ClyMethod asCalypsoItemContext
]
