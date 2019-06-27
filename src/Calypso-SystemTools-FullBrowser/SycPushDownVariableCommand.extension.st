Extension { #name : #SycPushDownVariableCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycPushDownVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2001 for: ClyVariableContextOfFullBrowser 
]
