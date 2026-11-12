Extension { #name : 'ClyFullBrowserClassGroupContext' }

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyFullBrowserClassGroupContext class >> calypsoSystemPluginsCriticBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	(aCommandMap contextMenu group: ClyQueryMenuGroup) add: ClyOpenCriticBrowserCommand
]
