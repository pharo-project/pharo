Extension { #name : 'ClyFullBrowserMethodGroupContext' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyFullBrowserMethodGroupContext class >> calypsoSystemToolsQueryBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap doubleClick add: ClyShowGroupsInMethodBrowserCommand) priority: 10000.
	(aCommandMap contextMenu group: ClyQueryMenuGroup)
		add: ClyShowClassRefCommand;
		add: ClyShowGroupsInMethodBrowserCommand;
		add: ClyShowPoolUsersCommand;
		add: ClyShowTraitUsersCommand
]
