Extension { #name : 'ClyFullBrowserClassGroupContext' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
ClyFullBrowserClassGroupContext class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableIcons add: ClyRunTestsFromPackagesCommand
]
