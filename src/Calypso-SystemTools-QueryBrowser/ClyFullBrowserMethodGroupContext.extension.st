Extension { #name : 'ClyFullBrowserMethodGroupContext' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyFullBrowserMethodGroupContext class >> calypsoSystemToolsQueryBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap doubleClick add: ClyShowGroupsInMethodBrowserCommand) priority: 10000
]
