Extension { #name : 'ClyFullBrowserPackageContext' }

{ #category : '*Calypso-SystemPlugins-Monticello-Browser' }
ClyFullBrowserPackageContext class >> calypsoSystemPluginsMonticelloBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: ClyCommitPackageCommand
]
