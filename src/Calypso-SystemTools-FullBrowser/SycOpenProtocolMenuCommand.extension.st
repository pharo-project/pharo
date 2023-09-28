Extension { #name : 'SycOpenProtocolMenuCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycOpenProtocolMenuCommand class >> fullBrowserMethodGroupMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 10002 for: ClyFullBrowserMethodGroupContext
]
