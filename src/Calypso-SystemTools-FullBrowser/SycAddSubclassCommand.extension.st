Extension { #name : #SycAddSubclassCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddSubclassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 2.9 for: ClyClassContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddSubclassCommand class >> fullBrowserTabActivation [
	<classAnnotation>
	
	^ClyBrowserTabCommandActivation for: ClyClassContextOfFullBrowser 
]
