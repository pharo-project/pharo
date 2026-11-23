Extension { #name : 'ClySystemBrowserContext' }

{ #category : '*Calypso-SystemPlugins-Deprecation-Browser' }
ClySystemBrowserContext class >> calypsoSystemPluginsDeprecationBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableDecorators add: ClyDeprecatedItemTableDecorator
]
