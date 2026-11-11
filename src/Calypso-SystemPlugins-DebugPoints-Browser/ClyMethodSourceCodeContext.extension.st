Extension { #name : 'ClyMethodSourceCodeContext' }

{ #category : '*Calypso-SystemPlugins-DebugPoints-Browser' }
ClyMethodSourceCodeContext class >> calypsoSystemPluginsDebugPointsBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap textLeftBarDoubleClick add: ClyAddBreakPointCommand
]
