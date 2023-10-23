Extension { #name : 'SycRealizeClassCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRealizeClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 2.1 for: ClyFullBrowserClassContext
]
