Extension { #name : 'ClyFullBrowserPackageContext' }

{ #category : '*Calypso-SystemPlugins-DependencyAnalyser-Browser' }
ClyFullBrowserPackageContext class >> calypsoSystemPluginsDependencyAnalyserBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClyQueryMenuGroup) add: ClyOpenDependencyBrowserCommand
]
