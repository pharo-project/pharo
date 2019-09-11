Extension { #name : #SycGenerateAllInstVarAccessorsCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycGenerateAllInstVarAccessorsCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2.1 for: ClyClassContextOfFullBrowser 
]
