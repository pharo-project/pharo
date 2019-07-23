Extension { #name : #SycPushDownMethodCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycPushDownMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: CmdRefactorMenuGroup 
		order: 31 
		for: ClyMethod asCalypsoItemContext
]
