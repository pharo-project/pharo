Extension { #name : 'ClyClassGroup' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
ClyClassGroup class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: ClyRunTestsFromClassGroupCommand.
	(aCommandMap tableIcons add: ClyRunTestsFromClassGroupCommand) priority: 1000.
	aCommandMap shortcuts add: ClyRunTestsFromClassGroupCommand key: $t meta
]
