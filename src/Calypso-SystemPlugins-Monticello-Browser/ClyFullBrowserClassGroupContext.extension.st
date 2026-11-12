Extension { #name : 'ClyFullBrowserClassGroupContext' }

{ #category : '*Calypso-SystemPlugins-Monticello-Browser' }
ClyFullBrowserClassGroupContext class >> calypsoSystemPluginsMonticelloBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: ClyCommitPackageCommand
]
