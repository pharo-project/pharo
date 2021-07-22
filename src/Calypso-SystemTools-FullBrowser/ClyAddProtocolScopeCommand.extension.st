Extension { #name : #ClyAddProtocolScopeCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyAddProtocolScopeCommand class >> fullBrowserMethodGroupMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyFullBrowserMethodGroupContext 
]
