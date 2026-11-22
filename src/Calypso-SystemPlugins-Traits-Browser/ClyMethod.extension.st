Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-Traits-Browser' }
ClyMethod class >> calypsoSystemPluginsTraitsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyTraitMethodDecorator
]
