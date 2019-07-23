Extension { #name : #SycTagMethodCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycTagMethodCommand class >> methodContextMenuActivation [ 
	<classAnnotation>

 	^ CmdContextMenuActivation 
		byItemOf: CmdRefactorMenuGroup 
		order: 2 
		for: ClyMethod asCalypsoItemContext
]
