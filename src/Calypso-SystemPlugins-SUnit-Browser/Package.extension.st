Extension { #name : 'Package' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
Package class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap contextMenu add: ClyRunTestsFromPackagesCommand.
	(aCommandMap tableIcons add: ClyRunTestsFromPackagesCommand) priority: 1000.
	aCommandMap shortcuts add: ClyRunTestsFromPackagesCommand key: $t meta
]
