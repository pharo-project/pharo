Extension { #name : 'ClyMethod' }

{ #category : '*Calypso-SystemPlugins-FFI-Browser' }
ClyMethod class >> calypsoSystemPluginsFFIBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyFFIMethodTableDecorator
]
