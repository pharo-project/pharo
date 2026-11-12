Extension { #name : 'ClyFullBrowserClassGroupContext' }

{ #category : '*Calypso-SystemPlugins-DependencyAnalyser-Browser' }
ClyFullBrowserClassGroupContext class >> calypsoSystemPluginsDependencyAnalyserBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClyQueryMenuGroup) add: ClyOpenDependencyBrowserCommand
]
