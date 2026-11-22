Extension { #name : 'ClyFullBrowserGlobalContext' }

{ #category : '*Calypso-SystemPlugins-Monticello-Browser' }
ClyFullBrowserGlobalContext class >> calypsoSystemPluginsMonticelloBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts add: ClyCommitPackageCommand key: $s meta
]
