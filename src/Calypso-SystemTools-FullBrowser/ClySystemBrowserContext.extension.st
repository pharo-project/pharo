Extension { #name : 'ClySystemBrowserContext' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClySystemBrowserContext class >> calypsoSystemToolsFullBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts add: ClySpawnFullBrowserCommand key: $b meta.
	aCommandMap contextMenu add: ClySwitchToScopedModeCommand.
	(aCommandMap contextMenu group: ClyQueryMenuGroup)
		add: ClyFindMethodInBrowserCommand;
		add: ClySpawnFullBrowserCommand
]
