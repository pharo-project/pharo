Extension { #name : #SycProtectVariableCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycProtectVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2000 for: ClyVariableContextOfFullBrowser 
]
