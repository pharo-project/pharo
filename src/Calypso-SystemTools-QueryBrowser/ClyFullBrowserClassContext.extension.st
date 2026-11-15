Extension { #name : 'ClyFullBrowserClassContext' }

{ #category : '*Calypso-SystemTools-QueryBrowser' }
ClyFullBrowserClassContext class >> calypsoSystemToolsQueryBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap toolbar group: ClyButtonToolbarGroup) add: ClyShowClassRefCommand order: 1000.
	aCommandMap shortcuts add: ClyShowClassRefCommand key: $n meta.
	(aCommandMap contextMenu group: ClyQueryMenuGroup)
		add: ClyShowClassRefCommand;
		add: ClyShowPoolUsersCommand;
		add: ClyShowTraitUsersCommand
]
