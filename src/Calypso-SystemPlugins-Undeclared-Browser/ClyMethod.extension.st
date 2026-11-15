Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-Undeclared-Browser' }
ClyMethod class >> calypsoSystemPluginsUndeclaredBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyUndeclaredMethodTableDecorator
]
