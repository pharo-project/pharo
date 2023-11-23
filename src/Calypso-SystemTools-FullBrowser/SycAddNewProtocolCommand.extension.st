Extension { #name : 'SycAddNewProtocolCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAddNewProtocolCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 2 for: ClyFullBrowserMethodGroupContext
]
