Extension { #name : #SycPushDownMethodCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycPushDownMethodCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2001 for: ClyMethodContextOfFullBrowser 
]
