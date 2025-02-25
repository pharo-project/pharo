Extension { #name : 'SycAbstractAllInstVarAccessorsCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAbstractAllInstVarAccessorsCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 2.1 for: ClyFullBrowserClassContext
]
