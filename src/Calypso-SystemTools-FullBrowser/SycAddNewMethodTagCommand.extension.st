Extension { #name : #SycAddNewMethodTagCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddNewMethodTagCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2 for: ClyMethodGroupContextOfFullBrowser 
]
