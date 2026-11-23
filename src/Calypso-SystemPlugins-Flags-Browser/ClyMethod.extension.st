Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-Flags-Browser' }
ClyMethod class >> calypsoSystemPluginsFlagsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyFlagMethodTableDecorator
]
