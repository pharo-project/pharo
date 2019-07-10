Extension { #name : #SycPushUpMethodCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycPushUpMethodCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2000 for: ClyMethodContextOfFullBrowser 
]
