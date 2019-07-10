Extension { #name : #SycCategorizeAllUnclassifiedMethodsCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycCategorizeAllUnclassifiedMethodsCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyMethodGroupContextOfFullBrowser 
]
