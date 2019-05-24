Extension { #name : #SycPushUpVariableCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycPushUpVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2000 for: ClyVariableContextOfFullBrowser 
]
