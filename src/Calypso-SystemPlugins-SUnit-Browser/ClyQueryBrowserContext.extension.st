Extension { #name : 'ClyQueryBrowserContext' }

{ #category : '*Calypso-SystemPlugins-SUnit-Browser' }
ClyQueryBrowserContext class >> calypsoSystemPluginsSUnitBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap toolbar group: ClyButtonToolbarGroup)
		add: ClyRunAllTestsFromMethodDataSourceCommand order: 10000;
		add: ClyRunFailedTestsFromMethodDataSourceCommand order: 10001
]
