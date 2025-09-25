Extension { #name : 'SycPushDownVariableCommand2' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycPushDownVariableCommand2 class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 2002 for: ClyFullBrowserVariableContext
]
