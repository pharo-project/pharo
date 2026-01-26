Extension { #name : 'SycViolentRemoveClassCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycViolentRemoveClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyFullBrowserClassContext
]
