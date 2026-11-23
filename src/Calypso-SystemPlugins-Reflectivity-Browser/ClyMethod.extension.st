Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyMethod class >> calypsoSystemPluginsReflectivityBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyHaltMethodTableDecorator
]
