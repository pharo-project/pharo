Extension { #name : #SycGenerateAllInstVarAccessorsCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycGenerateAllInstVarAccessorsCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdRefactorMenuGroup  
		order: 2  
		for: ClyClass asCalypsoItemContext 
]
