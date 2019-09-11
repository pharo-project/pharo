Extension { #name : #SycRemoveEmptyMethodTagsCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveEmptyMethodTagsCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 10001 for: ClyMethodGroupContextOfFullBrowser 
]
