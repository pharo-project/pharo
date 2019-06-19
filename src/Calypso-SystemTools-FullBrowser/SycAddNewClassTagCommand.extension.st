Extension { #name : #SycAddNewClassTagCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddNewClassTagCommand class >> fullBrowserClassGroupMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyClassGroupContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddNewClassTagCommand class >> fullBrowserPackageMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyPackageContextOfFullBrowser 
]
