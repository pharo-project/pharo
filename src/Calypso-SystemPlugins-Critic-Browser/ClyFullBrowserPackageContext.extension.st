Extension { #name : 'ClyFullBrowserPackageContext' }

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyFullBrowserPackageContext class >> calypsoSystemPluginsCriticBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClyQueryMenuGroup) add: ClyOpenCriticBrowserCommand
]
