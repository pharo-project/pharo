Extension { #name : #SycNewClassCreationCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycNewClassCreationCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 3 for: ClyClassContextOfFullBrowser 
]
